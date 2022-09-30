{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mypretty (
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

bindir     = "C:\\Program Files\\Haskell\\bin"
libdir     = "C:\\Program Files\\Haskell\\x86_64-windows-ghc-8.10.7\\mypretty-0.1.0.0-5eWsVVax2Zf5E6uS2dovOO"
dynlibdir  = "C:\\Program Files\\Haskell\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Program Files\\Haskell\\x86_64-windows-ghc-8.10.7\\mypretty-0.1.0.0"
libexecdir = "C:\\Program Files\\Haskell\\mypretty-0.1.0.0-5eWsVVax2Zf5E6uS2dovOO\\x86_64-windows-ghc-8.10.7\\mypretty-0.1.0.0"
sysconfdir = "C:\\Program Files\\Haskell\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mypretty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mypretty_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mypretty_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mypretty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mypretty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mypretty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
