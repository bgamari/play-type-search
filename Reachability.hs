module Reachability (unreachableBinders) where

import Data.Maybe (mapMaybe)
import Data.Foldable

import qualified GHC
import           GHC (GenLocated(L))
import qualified Digraph
import VarEnv
import VarSet

import Utils

usageGraph :: [GHC.TypecheckedSource] -> Digraph.Graph (Digraph.Node GHC.Id ())
usageGraph = Digraph.graphFromEdgedVertices . foldMap (foldMap doBind)
  where
    doBind :: GHC.LHsBind GHC.Id -> [Digraph.Node GHC.Id ()]
    doBind bind@(L _ (GHC.FunBind { GHC.fun_id=L _ ident
                                  , GHC.fun_matches=matches
                                  })) =
        [((), ident, usedBinders)]
      where
        usedBinders = everythingM binderList matches
        binderList :: GHC.HsExpr GHC.Id -> [GHC.Id]
        binderList (GHC.HsVar x) = [x]
        binderList _             = []
    doBind (L _ (GHC.AbsBinds { GHC.abs_binds = binds })) =
        foldMap doBind binds
    doBind _ = mempty

unreachableBinders :: String -- ^ root module
                   -> [GHC.TypecheckedModule] -> GHC.Ghc ()
unreachableBinders root tcms = do
    rootBinders <- return [] -- TODO
    let graph = usageGraph $ toList $ fmap (GHC.tm_typechecked_source) tcms
        getId :: Digraph.Node GHC.Id () -> GHC.Id
        getId (_,ident,_) = ident
        reachable = foldMap (foldMap (unitVarSet . getId) . Digraph.reachableG graph) rootBinders
        all = mkVarSet $ map getId $ Digraph.verticesG graph
        unreachable = all `minusVarSet` reachable
        binders = definedBinders $ toList $ fmap (GHC.tm_typechecked_source) tcms
    printBindings $ mapMaybe (binders `lookupVarEnv`) $ varSetElems unreachable
