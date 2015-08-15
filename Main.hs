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

import Usage
import Reachability
import Utils

data Opts = Opts { mode        :: [GHC.TypecheckedModule] -> GHC.Ghc ()
                 , distDir     :: Maybe String
                 , sourceFiles :: [FilePath]
                 , verbose     :: Verbosity.Verbosity
                 }

newtype Matcher = Matcher
    { runMatcher :: forall r. Monoid r => (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> GHC.Ghc r }

pureMatcher :: (a -> GHC.Ghc b)
            -> (forall r. Monoid r => b -> (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> r)
            -> a -> Matcher
pureMatcher prepare match x =
    Matcher $ \f binds -> prepare x >>= \y -> pure $ match y f binds

opts = Opts
       <$> (matchMode <|> deadCodeMode)
       <*> optional (strOption $ long "builddir" <> metavar "DIR" <> help "cabal dist/ directory")
       <*> many (strArgument $ metavar "MODULE.hs" <> help "Haskell source modules to search within")
       <*> option (maybe mzero pure . Verbosity.intToVerbosity =<< auto)
                  (short 'v' <> long "verbose" <> metavar "N" <> help "Verbosity level"
                   <> value Verbosity.normal)
  where
    matchMode = fmap printMatchingBindings
                $ typeContains <|> typeContainsCon <|> ofType
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

    deadCodeMode = unreachableBinders
                   <$> strOption (  long "dead-code"
                                 <> help "Find unreachable code")

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

main = do
    args <- execParser $ info (helper <*> opts) mempty
    GHC.runGhc (Just GHC.Paths.libdir) (ghcMain args)

ghcMain :: Opts -> GHC.Ghc ()
ghcMain args = GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
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
    --printSDoc $ ppr $ usageGraph (map GHC.tm_typechecked_source (toList typechecked))

    (mode args) (toList typechecked)

printMatchingBindings :: Matcher -> [GHC.TypecheckedModule] -> GHC.Ghc ()
printMatchingBindings matcher tcms = do
    matches <- concat <$> mapM (runMatcher matcher (\a->[a])
                                . GHC.tm_typechecked_source)
                               tcms
    printBindings matches


-- TODO:
--   Construction/destruction queries
