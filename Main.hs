{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !MIN_VERSION_base(4,8,0)
import Prelude hiding (mapM, concat)
import Data.Traversable (traverse, mapM)
#endif

import Data.Monoid
import Data.Maybe (fromMaybe)
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

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.Simple.Utils as Utils
import GHC.Cabal

newtype Matcher = Matcher
    { runMatcher :: forall r. Monoid r => (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> GHC.Ghc r }

data Opts = Opts { matcher     :: Matcher
                 , distDir     :: Maybe String
                 , sourceFiles :: [FilePath]
                 , verbose     :: Verbosity.Verbosity
                 }

pureMatcher :: (a -> GHC.Ghc b)
            -> (forall r. Monoid r => b -> (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> r)
            -> a -> Matcher
pureMatcher prepare match x =
    Matcher $ \f binds -> prepare x >>= \y -> pure $ match y f binds

opts = Opts
       <$> matchers
       <*> optional (strOption $ long "builddir" <> metavar "DIR" <> help "cabal dist/ directory")
       <*> many (strArgument $ metavar "MODULE.hs" <> help "Haskell source modules to search within")
       <*> option (maybe mzero pure . Verbosity.intToVerbosity =<< auto)
                  (short 'v' <> long "verbose" <> metavar "N" <> help "Verbosity level"
                   <> value Verbosity.normal)
  where
    matchers = typeContains <|> typeContainsCon <|> ofType
    typeContains = pureMatcher (withExplicitForAll . lookupType) foldBindsContainingType
                   <$> strOption (  long "containing"
                                 <> help "Find bindings whose type mentions the given type"
                                 <> metavar "TYPE")
    typeContainsCon = pureMatcher lookupTyCon foldBindsContainingTyCon
                      <$> strOption (  long "containing-con"
                                    <> help "Find bindings whose type mentions the given type constructor"
                                    <> metavar "TYPECON")
    ofType = pureMatcher (withExplicitForAll . lookupType) foldBindsOfType
             <$> strOption (   long "of-type"
                            <> help "Find bindings of the given type"
                            <> metavar "TYPE")

main = do
    args <- execParser $ info (helper <*> opts) mempty
    GHC.runGhc (Just GHC.Paths.libdir) (runMatch args)

setupDynFlags :: Opts -> GHC.Ghc GHC.DynFlags
setupDynFlags args = session >> GHC.getSessionDynFlags
  where
    session = do
        -- Note that this initial {get,set}SessionDynFlags is not idempotent
        dflags <- GHC.getSessionDynFlags
        (dflags', cd) <- maybe (dflags, Nothing) (\(a,b)->(a, Just b))
                         <$> liftIO (initCabalDynFlags (verbose args) (distDir args) dflags)
        GHC.setSessionDynFlags dflags' { hscTarget = HscNothing }

        let targets = fmap (componentTargets . cdComponent) cd
        liftIO $ Utils.debug (verbose args) $ showSDoc dflags
          $ text "Targets" <+> ppr targets
        traverse GHC.setTargets targets

-- | Run a 'Ghc' action in a modified environment with 'Opt_ExplicitForAll'
-- enabled in the interactive 'DynFlags'
withExplicitForAll :: GHC.Ghc a -> GHC.Ghc a
withExplicitForAll =
    withTempSession $ modifyIC (modifyDynFlags $ flip xopt_set Opt_ExplicitForAll)
  where
    modifyIC f env = env { HscTypes.hsc_IC = f (HscTypes.hsc_IC env) }

    modifyDynFlags :: ContainsDynFlags t => (DynFlags -> DynFlags) -> t -> t
    modifyDynFlags f env = replaceDynFlags env (f $ extractDynFlags env)

instance ContainsDynFlags HscTypes.InteractiveContext where
    extractDynFlags = HscTypes.ic_dflags
    replaceDynFlags ic dflags = ic {HscTypes.ic_dflags = dflags}

runMatch :: Opts -> GHC.Ghc ()
runMatch args = GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    dflags <- setupDynFlags args
    let printSDoc :: SDoc -> GHC.Ghc ()
        printSDoc = liftIO . putStrLn . showSDoc dflags
        debugSDoc :: SDoc -> GHC.Ghc ()
        debugSDoc = liftIO . Utils.debug (verbose args) . showSDoc dflags

    targets <- mapM (\s -> GHC.guessTarget s Nothing) (sourceFiles args)
    GHC.getTargets >>= GHC.setTargets . (++targets)

    summaries <- GHC.depanal [] True
    let graph = flattenSCCs $ GHC.topSortModuleGraph True summaries Nothing
    let processModule :: GHC.ModSummary -> GHC.Ghc GHC.TypecheckedModule
        processModule ms = do
            tcd <- GHC.parseModule ms >>= GHC.typecheckModule
            GHC.loadModule tcd
            return tcd
    typechecked <- mapM processModule graph
    let modNames = map (GHC.moduleName . GHC.ms_mod) graph
    GHC.setContext $ map GHC.IIModule modNames

    debugSDoc $ vcat $ map (ppr . GHC.tm_typechecked_source) (toList typechecked)
    printSDoc $ ppr $ deadBindings (map GHC.tm_typechecked_source (toList typechecked))
    printSDoc $ ppr $ usageGraph (map GHC.tm_typechecked_source (toList typechecked))

    matches <- concat <$> mapM (runMatcher (matcher args) (\a->[a])
                                . GHC.tm_typechecked_source)
                               (toList typechecked)
    printSDoc $ vcat $ map pprLocated matches

pprLocated :: (Outputable l, Outputable e) => GHC.GenLocated l e -> SDoc
pprLocated (L l e) = braces (ppr l) $$ nest 4 (ppr e)

-- | Thows error if not in scope
lookupType :: String -> GHC.Ghc GHC.Type
lookupType tyName = fst <$> GHC.typeKind False tyName

-- | aw
lookupTyCon :: String -> GHC.Ghc GHC.TyCon
lookupTyCon tyConName = getTyCon <$> lookupType tyConName
  where
    getTyCon (TyConApp tyCon _) = tyCon
    getTyCon _                  =
      error "lookupTyCon: Expected type constructor application"

foldBindsOfType :: (Monoid r)
                => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                -> GHC.LHsBinds GHC.Id -> r
foldBindsOfType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | GHC.idType fid `Type.eqType` ty = f bind
    go _ = mempty

foldBindsContainingType :: Monoid r
                        => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                        -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | getAny $ everything mappend (mempty `mkQ` containsType) (GHC.idType fid) = f bind
      where
        -- a type variable will unify with anything
        --containsType (TyVarTy _)       = mempty

        -- We first check whether the types match, allowing all type variables to vary.
        -- This, however, is too lenient: the matcher is free to introduce equalities
        -- between our template variables. So, if this matches we then take the
        -- resulting substitution
        containsType ty'
          | Just subst <- Unify.tcMatchTy tyVars strippedTy ty'
          , bijectiveSubst subst
                      = --trace (showSDoc unsafeGlobalDynFlags $ ppr (strippedTy, ty', Type.tyVarsOfType ty', subst, tyVars)) $
                        Any True
          | otherwise = --trace (showSDoc unsafeGlobalDynFlags $ ppr
                        --       $ let subst = Unify.tcMatchTy tyVars strippedTy ty'
                        --         in (subst, Type.tyVarsOfType ty'))
                        mempty
          where
            bijectiveSubst :: Type.TvSubst -> Bool
            bijectiveSubst (Type.TvSubst _ subst) = iter emptyVarSet (varEnvElts subst)
              where
                iter :: VarSet -> [Type] -> Bool
                iter _             []                = True
                iter claimedTyVars (TypeRep.TyVarTy tyVar:rest)
                  | tyVar `elemVarSet` claimedTyVars = False
                  | otherwise                        = iter (extendVarSet claimedTyVars tyVar) rest
                iter claimedTyVars (_:rest)          = iter claimedTyVars rest
    go _ = mempty

    -- We don't necessarily want to match on the foralls the user needed to
    -- merely bring type variables into scope
    stripForAlls :: VarSet -> Type -> (Type, VarSet)
    stripForAlls vars (ForAllTy var ty) = stripForAlls (VarSet.extendVarSet vars var) ty
    stripForAlls vars ty                = (ty, vars)
    (strippedTy, tyVars) = stripForAlls VarSet.emptyVarSet ty

    isPrimed var = "'" `isSuffixOf` OccName.occNameString (OccName.occName $ Var.varName var)
    templVars = VarSet.filterVarSet (not . isPrimed) tyVars

foldBindsContainingTyCon :: Monoid r
                        => GHC.TyCon -> (GHC.LHsBind GHC.Id -> r)
                        -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingTyCon tyCon f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | getAny $ everything mappend (mempty `mkQ` containsTyCon) (GHC.idType fid) = f bind
      where
        containsTyCon tyCon' | tyCon == tyCon' = Any True
        containsTyCon _                        = mempty
    go _ = mempty

foldBindsContainingIdent :: Monoid r
                         => GHC.Id -> (GHC.LHsBind GHC.Id -> r)
                         -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingIdent ident f = everything mappend (mempty `mkQ` go)
  where
    go bind
      | getAny $ everything mappend (mempty `mkQ` containsId) bind = f bind
      where
        containsId ident' | ident == ident' = Any True
        containsTyCon _                     = mempty
    go _ = mempty

-- | Recursive top-down query
everythingM :: (Monoid r, Typeable b, Data a) => (b -> r) -> a -> r
everythingM f = everything mappend (mempty `mkQ` f)

gmapQlM :: (Monoid r, Typeable b, Data a) => (b -> r) -> a -> r
gmapQlM f = gmapQl mappend mempty (mempty `mkQ` f)

data Pair a b = Pair !a !b
instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty
    Pair a b `mappend` Pair x y = Pair (a `mappend` x) (b `mappend` y)

-- | A set of identifiers and their associated binding
type BindSet = VarEnv (GHC.LHsBind GHC.Id)

usageGraph :: [GHC.TypecheckedSource] -> Digraph.Graph (Digraph.Node GHC.Id ())
usageGraph = Digraph.graphFromEdgedVertices . foldMap (foldMap bind . bagToList)
  where
    bind :: GHC.LHsBind GHC.Id -> [Digraph.Node GHC.Id ()]
    bind bind@(L _ (GHC.FunBind { GHC.fun_id=L _ id
                                , GHC.fun_matches=matches
                                })) =
        [((), id, usedBinders)]
      where
        usedBinders = everythingM binderList matches
        binderList :: GHC.HsExpr GHC.Id -> [GHC.Id]
        binderList (GHC.HsVar x) = [x]
        binderList _             = []
    bind (L _ (GHC.AbsBinds { GHC.abs_binds = binds })) =
        foldMap bind (bagToList binds)
    bind _ = mempty

-- | Identify the dead bindings in a source tree
deadBindings :: [GHC.TypecheckedSource] -> [GHC.LHsBind GHC.Id]
deadBindings mods =
    varEnvElts $ fold $ takeWhile (not . isEmptyVarEnv) $ iterate go emptyVarEnv
  where
    -- find all defined top-level binders
    definedBinders :: BindSet
    definedBinders = foldMap (foldMap binder . bagToList) mods
      where
        binder :: GHC.LHsBind GHC.Id -> BindSet
        binder bind@(L _ (GHC.FunBind {GHC.fun_id=L _ id})) = unitVarEnv id bind
        binder _                                            = mempty

    findUsedBinders :: BindSet -> VarSet
    findUsedBinders knownDead = everythingWithTrunc mappend ((mempty, True) `mkQ` used) mods
      where
        -- the set of binders used by the given binding
        used :: GHC.LHsBind GHC.Id -> (VarSet, Bool)
        used (L _ (GHC.FunBind {GHC.fun_id=L _ id, GHC.fun_matches=matches}))
          -- we don't care what a binding uses if it is already known to be dead
          | id `elemVarEnv` knownDead = (mempty, False)
          | otherwise                 = (usedBinders, True)
          where
            -- we don't care about recursive uses
            usedBinders = everythingM usage matches `delVarEnv` id
              where
                usage :: GHC.HsExpr GHC.Id -> VarSet
                usage (GHC.HsVar x) = unitVarSet x
                usage _             = emptyVarSet
        used bind = (usedBinders, True) where usedBinders = everythingM unitVarSet bind

    go :: BindSet -> BindSet
    go knownDead = definedBinders `minusVarEnv` findUsedBinders knownDead


-- TODO:
--   Construction/destruction queries


-- | Summarise all nodes in top-down, left-to-right order, potentially
-- truncating some branches of the tree.
--
-- Return 'False' to truncate traversal.
everythingWithTrunc :: forall r. (r -> r -> r) -> GenericQ (r, Bool) -> GenericQ r
everythingWithTrunc f q = go
  where
    go :: Data a => a -> r
    go x
      | s         = foldl f r (gmapQ go x)
      | otherwise = r
      where (r, s) = q x
