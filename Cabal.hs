-- | One of the challenges of using the GHC API for external tooling
-- is handling integration with Cabal. This library provides a simple
-- interface for configuring GHC's 'DynFlags' as Cabal would have,
-- allowing seamless tooling use on Cabal projects.

module Cabal (
      -- * Initializing GHC DynFlags for Cabal packages
      initCabalDynFlags, initCabalDynFlags'
      -- * Re-exports
    , Verbosity
    ) where

import Control.Monad (guard, msum, mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class

import Control.Applicative ((<|>))
import Distribution.Verbosity
import Distribution.Simple.Utils (defaultPackageDesc, warn, debug, findPackageDesc)
import Distribution.Simple.Program (defaultProgramConfiguration)
import qualified Distribution.Simple.Setup as Setup
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse as PD
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Simple.GHC      as CGHC
import qualified Distribution.Simple.Program.GHC as CGHC
import DynFlags (DynFlags, parseDynamicFlagsCmdLine)
import qualified SrcLoc

data CabalDetails = CabalDetails { cdLocalBuildInfo :: LocalBuildInfo
                                 }

initCabalDynFlags :: Verbosity -> DynFlags -> IO (Maybe DynFlags)
initCabalDynFlags verbosity dflags0 = runMaybeT $ do
    let warnNoCabal _err = lift (warn verbosity "Couldn't find cabal file") >> mzero
    pdfile <- either warnNoCabal pure =<< lift (findPackageDesc ".")
    gpkg_descr <- lift $ PD.readPackageDescription verbosity pdfile

    --let pkgDbs = reverse $ map Just [Compiler.GlobalPackageDB, Compiler.UserPackageDB]
    --let cflags = (Setup.defaultConfigFlags defaultProgramConfiguration) {Setup.configPackageDBs=pkgDbs}
    --print cflags
    --lbi <- Configure.configure (gpkg_descr, PD.emptyHookedBuildInfo) cflags
    lbi <- lift $ Configure.getPersistBuildConfig Setup.defaultDistPref

    let programsConfig = defaultProgramConfiguration
    (comp, compPlatform, programsConfig') <- lift $
        Configure.configCompilerEx (Just Compiler.GHC) Nothing Nothing
                                   (withPrograms lbi) (lessVerbose verbosity)

    -- TODO: is any of this correct?
    let pkg_descr = case finalizePackageDescription
                             [] (const True) compPlatform (Compiler.compilerInfo comp)
                             [] gpkg_descr of
                        Right (pd,_) -> pd
                        -- This shouldn't happen since we claim dependencies can always be satisfied
                        Left err     -> error "missing dependencies"
    let comp :: Maybe (PD.BuildInfo, LBI.ComponentLocalBuildInfo)
        comp = msum [libraryComp, executableComp]

        libraryComp = do
            lib <- PD.library pkg_descr
            let bi = PD.libBuildInfo lib
            guard $ PD.buildable bi
            return (bi, getComponentLocalBuildInfo lbi CLibName)

        executableComp = msum $ flip map (PD.executables pkg_descr) $ \exec->do
            let bi = PD.buildInfo exec
            guard $ PD.buildable bi
            return (bi, getComponentLocalBuildInfo lbi (LBI.CExeName $ PD.exeName exec))

    case comp of
      Just (bi, clbi) -> lift $ initCabalDynFlags' verbosity lbi bi clbi dflags0
      Nothing         -> do
          lift $ warn verbosity $ "Found no buildable components in "++pdfile
          mzero

initCabalDynFlags' :: Verbosity -> LocalBuildInfo
                   -> BuildInfo -> ComponentLocalBuildInfo
                   -> DynFlags -> IO DynFlags
initCabalDynFlags' verbosity lbi bi clbi dflags0 = do
    debug verbosity $ "initCabalDynFlags': Flags = "++show rendered
    (dflags, leftovers, warnings) <- DynFlags.parseDynamicFlagsCmdLine dflags0 (map SrcLoc.noLoc rendered)
    putStrLn $ unlines $ map SrcLoc.unLoc warnings
    return dflags
  where
    baseOpts = CGHC.componentGhcOptions verbosity lbi bi clbi (buildDir lbi)
    rendered = CGHC.renderGhcOptions (LBI.compiler lbi) baseOpts
