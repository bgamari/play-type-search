module Reachability (unreachableBinders) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Generics
import Data.Foldable
import Data.List (isSuffixOf)
import Control.Monad (mzero)
import Control.Monad.IO.Class

import DynFlags
import GhcMonad (withTempSession)
import qualified GHC
import           GHC (GenLocated(L), unLoc)
import qualified GHC.Paths
import qualified TypeRep
import           TypeRep (Type(..))
import qualified Unify
import qualified OccName
import qualified Var
import qualified Type
import Digraph (flattenSCCs) -- this should be expected from GHC
import qualified Digraph
import Outputable hiding ((<>))
import VarEnv
import VarSet
import Bag
import qualified HscTypes

import Options.Applicative hiding ((<>))
import Utils

usageGraph :: [GHC.TypecheckedSource] -> Digraph.Graph (Digraph.Node GHC.Id ())
usageGraph = Digraph.graphFromEdgedVertices . foldMap (foldMap doBind)
  where
    doBind :: GHC.LHsBind GHC.Id -> [Digraph.Node GHC.Id ()]
    doBind bind@(L _ (GHC.FunBind { GHC.fun_id=L _ id
                                  , GHC.fun_matches=matches
                                  })) =
        [((), id, usedBinders)]
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
        getId (_,id,_) = id
        reachable = foldMap (foldMap (unitVarSet . getId) . Digraph.reachableG graph) rootBinders
        all = mkVarSet $ map getId $ Digraph.verticesG graph
        unreachable = all `minusVarSet` reachable
        binders = definedBinders $ toList $ fmap (GHC.tm_typechecked_source) tcms
    printBindings $ mapMaybe (binders `lookupVarEnv`) $ varSetElems unreachable

