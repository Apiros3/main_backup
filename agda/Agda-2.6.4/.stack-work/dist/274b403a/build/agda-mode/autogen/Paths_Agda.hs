{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Agda (
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
version = Version [2,6,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\bin"
libdir     = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\lib\\x86_64-windows-ghc-8.10.7\\Agda-2.6.4-g3PcsNd41Z1JCw1qj41Dh-agda-mode"
dynlibdir  = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\share\\x86_64-windows-ghc-8.10.7\\Agda-2.6.4"
libexecdir = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\libexec\\x86_64-windows-ghc-8.10.7\\Agda-2.6.4"
sysconfdir = "C:\\Users\\lemon\\Coding\\agda\\Agda-2.6.4\\.stack-work\\install\\06d5a46e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Agda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Agda_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Agda_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Agda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Agda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Agda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
