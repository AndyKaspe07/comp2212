{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_coursework (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/bin"
libdir     = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/lib/aarch64-osx-ghc-9.8.4/coursework-0.1.0.0-3r5sQx5hn335mTK5UdQubA"
dynlibdir  = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/share/aarch64-osx-ghc-9.8.4/coursework-0.1.0.0"
libexecdir = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/libexec/aarch64-osx-ghc-9.8.4/coursework-0.1.0.0"
sysconfdir = "/Users/andykaspe/Documents/GitHub/comp2212/submission/sift/.stack-work/install/aarch64-osx/03910f57f563c0bf8a62b5d1b7883a1cf0282e96d9daa4f56c07572ffde326eb/9.8.4/etc"

getBinDir     = catchIO (getEnv "coursework_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "coursework_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "coursework_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "coursework_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "coursework_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "coursework_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
