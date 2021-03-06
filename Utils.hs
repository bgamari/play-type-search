module Utils where

import Data.Generics
import Control.Monad.IO.Class

import DynFlags
import qualified GHC
import           GHC (GenLocated(L))
import           TypeRep (Type(..))
import Outputable hiding ((<>))
import VarEnv
import Bag

printBindings :: [GHC.LHsBind GHC.Id] -> GHC.Ghc ()
printBindings binds = do
    dflags <- getDynFlags
    let printSDoc = liftIO . putStrLn . showSDoc dflags
    printSDoc $ vcat $ map pprLocated binds

pprLocated :: (Outputable l, Outputable e) => GHC.GenLocated l e -> SDoc
pprLocated (L l e) = braces (ppr l) $$ nest 4 (ppr e)

-- | Thows error if not in scope
lookupType :: String -> GHC.Ghc GHC.Type
lookupType tyName = fst <$> GHC.typeKind False tyName

-- | Lookup a type constructor
lookupTyCon :: String -> GHC.Ghc GHC.TyCon
lookupTyCon tyConName = getTyCon <$> lookupType tyConName
  where
    getTyCon (TyConApp tyCon _) = tyCon
    getTyCon _                  =
      error "lookupTyCon: Expected type constructor application"

-- | Recursive top-down query
everythingM :: (Monoid r, Typeable b, Data a) => (b -> r) -> a -> r
everythingM f = everything mappend (mempty `mkQ` f)

-- | Construct a 'VarEnv' mapping top-level binders to their bindings.
definedBinders :: [GHC.TypecheckedSource] -> VarEnv (GHC.LHsBind GHC.Id)
definedBinders = foldMap (foldMap doBind)
  where
    doBind :: GHC.LHsBind GHC.Id -> VarEnv (GHC.LHsBind GHC.Id)
    doBind bind@(L _ (GHC.FunBind { GHC.fun_id=L _ id })) =
        unitVarEnv id bind
    doBind (L _ (GHC.AbsBinds { GHC.abs_binds = binds })) =
        foldMap doBind binds
    doBind _ = mempty

instance Foldable Bag where
    foldr = foldrBag
