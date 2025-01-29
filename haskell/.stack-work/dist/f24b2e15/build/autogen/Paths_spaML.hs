{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_spaML (
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
bindir     = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\bin"
libdir     = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\lib\\x86_64-windows-ghc-9.8.4\\spaML-0.1.0.0-3fl6lg9KTj1CPh9lJP7H5n"
dynlibdir  = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\share\\x86_64-windows-ghc-9.8.4\\spaML-0.1.0.0"
libexecdir = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\libexec\\x86_64-windows-ghc-9.8.4\\spaML-0.1.0.0"
sysconfdir = "C:\\Users\\vinic\\OneDrive\\UFCG\\24.2\\plp\\projetoHaskell\\spaML\\.stack-work\\install\\704423c5\\etc"

getBinDir     = catchIO (getEnv "spaML_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "spaML_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "spaML_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "spaML_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spaML_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spaML_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
