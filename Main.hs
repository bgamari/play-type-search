import Data.Monoid
import Data.Generics
import Data.Foldable
import Control.Monad.IO.Class
import DynFlags
import qualified GHC
import qualified GHC.Paths
import Digraph (flattenSCCs) -- this should be expected from GHC

import Outputable

main = do
    GHC.runGhc (Just $ GHC.Paths.libdir) action

action :: GHC.Ghc ()
action = GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- Note that this initial {get,set}SessionDynFlags is not idempotent
    dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags dflags { hscTarget = HscNothing }
    let printSDoc :: SDoc -> GHC.Ghc ()
        printSDoc = liftIO . putStrLn . showSDoc dflags

    GHC.setTargets [GHC.Target (GHC.TargetFile "Test.hs" Nothing) True Nothing]
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

    (ty, _) <- GHC.typeKind False "TestType"
    (ty', _) <- GHC.typeKind False "ThisIsAType"
    printSDoc $ vcat $ concatMap (map pprLocated
                                  . foldBindsOfType ty (\a->[a])
                                  . GHC.tm_typechecked_source)
                     $ toList typechecked

    printSDoc $ vcat $ concatMap (map pprLocated
                                  . foldBindsContainingType ty (\a->[a])
                                  . GHC.tm_typechecked_source)
                     $ toList typechecked

pprLocated :: (Outputable l, Outputable e) => GHC.GenLocated l e -> SDoc
pprLocated (GHC.L l e) = braces (ppr l) $$ nest 4 (ppr e)

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
