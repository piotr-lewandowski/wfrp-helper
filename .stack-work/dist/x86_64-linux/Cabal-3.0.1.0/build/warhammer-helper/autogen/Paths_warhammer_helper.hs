{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_warhammer_helper (
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

bindir     = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/bin"
libdir     = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/lib/x86_64-linux-ghc-8.8.4/warhammer-helper-0.1.0.0-D2vZbNF3a4REJUmVAwcbjy-warhammer-helper"
dynlibdir  = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/share/x86_64-linux-ghc-8.8.4/warhammer-helper-0.1.0.0"
libexecdir = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/libexec/x86_64-linux-ghc-8.8.4/warhammer-helper-0.1.0.0"
sysconfdir = "/mnt/d/Sajens/Haskell/warhammer-helper/.stack-work/install/x86_64-linux/5ba344c326608ad313f672c7e533e549f94f79b84416125e0c6a4375c7b02b2e/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warhammer_helper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warhammer_helper_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warhammer_helper_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warhammer_helper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warhammer_helper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warhammer_helper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
