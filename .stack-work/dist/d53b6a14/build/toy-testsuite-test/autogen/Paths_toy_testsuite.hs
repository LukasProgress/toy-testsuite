{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_toy_testsuite (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\bin"
libdir     = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\lib\\x86_64-windows-ghc-9.0.2\\toy-testsuite-0.1.0.0-DN4EfaeCPefLfmqhzschko-toy-testsuite-test"
dynlibdir  = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\share\\x86_64-windows-ghc-9.0.2\\toy-testsuite-0.1.0.0"
libexecdir = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\libexec\\x86_64-windows-ghc-9.0.2\\toy-testsuite-0.1.0.0"
sysconfdir = "D:\\StudiumsZeug\\Bachelorarbeit\\toy-testsuite\\.stack-work\\install\\f6f92701\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toy_testsuite_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toy_testsuite_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "toy_testsuite_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "toy_testsuite_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toy_testsuite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toy_testsuite_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
