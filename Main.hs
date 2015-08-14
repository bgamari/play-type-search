{-# LANGUAGE RankNTypes #-}

import Data.Monoid
import Data.Generics
import Data.Foldable
import Control.Monad.IO.Class
import DynFlags
import qualified GHC
import qualified GHC.Paths
import Digraph (flattenSCCs) -- this should be expected from GHC
import Options.Applicative hiding ((<>))

import Outputable hiding ((<>))

newtype Matcher = Matcher
    { runMatcher :: forall r. Monoid r => (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> GHC.Ghc r }

data Opts = Opts { matcher     :: Matcher
                 , sourceFiles :: [FilePath]
                 }

pureMatcher :: (a -> GHC.Ghc b)
            -> (forall r. Monoid r => b -> (GHC.LHsBind GHC.Id -> r) -> GHC.LHsBinds GHC.Id -> r)
            -> a -> Matcher
pureMatcher prepare match x =
    Matcher $ \f binds -> prepare x >>= \y -> pure $ match y f binds

opts = Opts
       <$> matchers
       <*> many (strArgument $ metavar "MODULE.hs" <> help "Haskell source modules to search within")
  where
    matchers = typeContains <|> ofType
    typeContains = pureMatcher lookupType foldBindsContainingType
                   <$> strOption (  long "containing"
                                 <> help "Find bindings whose type mentions the given type"
                                 <> metavar "TYPE")
    ofType = pureMatcher lookupType foldBindsOfType
             <$> strOption (   long "of-type"
                            <> help "Find bindings of the given type"
                            <> metavar "TYPE")

main = do
    args <- execParser $ info (helper <*> opts) mempty
    GHC.runGhc (Just GHC.Paths.libdir) (runMatch args)

runMatch :: Opts -> GHC.Ghc ()
runMatch args = GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- Note that this initial {get,set}SessionDynFlags is not idempotent
    dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags dflags { hscTarget = HscNothing }
    let printSDoc :: SDoc -> GHC.Ghc ()
        printSDoc = liftIO . putStrLn . showSDoc dflags

    GHC.setTargets (map (\f -> GHC.Target (GHC.TargetFile f Nothing) True Nothing) $ sourceFiles args)
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

    matches <- concat <$> mapM (runMatcher (matcher args) (\a->[a])
                                . GHC.tm_typechecked_source)
                               (toList typechecked)
    printSDoc $ vcat $ map pprLocated matches

pprLocated :: (Outputable l, Outputable e) => GHC.GenLocated l e -> SDoc
pprLocated (GHC.L l e) = braces (ppr l) $$ nest 4 (ppr e)

-- | Thows error if not in scope
lookupType :: String -> GHC.Ghc GHC.Type
lookupType tyName = fst <$> GHC.typeKind False tyName

foldBindsOfType :: (Monoid r)
                => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                -> GHC.LHsBinds GHC.Id -> r
foldBindsOfType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(GHC.L _ (GHC.FunBind {GHC.fun_id=GHC.L _ fid}))
      | GHC.idType fid == ty = f bind
    go _ = mempty

foldBindsContainingType :: Monoid r
                        => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                        -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(GHC.L _ (GHC.FunBind {GHC.fun_id=GHC.L _ fid}))
      | getAny $ everything mappend (mempty `mkQ` containsType) (GHC.idType fid) = f bind
      where containsType ty' | ty == ty' = Any True
            containsType _               = mempty
    go _ = mempty
