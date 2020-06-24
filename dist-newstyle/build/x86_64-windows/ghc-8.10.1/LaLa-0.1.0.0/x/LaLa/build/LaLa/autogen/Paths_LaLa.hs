{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_LaLa (
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

bindir     = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.1\\LaLa-0.1.0.0-inplace-LaLa"
dynlibdir  = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.1"
datadir    = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.1\\LaLa-0.1.0.0"
libexecdir = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\LaLa-0.1.0.0-inplace-LaLa\\x86_64-windows-ghc-8.10.1\\LaLa-0.1.0.0"
sysconfdir = "C:\\Users\\gelin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LaLa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LaLa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "LaLa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "LaLa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LaLa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LaLa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
