{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tutorial2 (
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

bindir     = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/bin"
libdir     = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/lib/x86_64-osx-ghc-8.10.7/tutorial2-0.1.0.0-AnHi9ZZa7Qx1u2kNjpxrnC-tutorial2-exe"
dynlibdir  = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/share/x86_64-osx-ghc-8.10.7/tutorial2-0.1.0.0"
libexecdir = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/libexec/x86_64-osx-ghc-8.10.7/tutorial2-0.1.0.0"
sysconfdir = "/Users/krishnakothandaraman/Documents/UG_courses/year4sem2/popl/code/tutorials/tutorial2/.stack-work/install/x86_64-osx/2f3e3b1a774c7d9888c142019d5bd9fa8587c3c646ca08f670e71ad4170e6e7b/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tutorial2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tutorial2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tutorial2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tutorial2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tutorial2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tutorial2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
