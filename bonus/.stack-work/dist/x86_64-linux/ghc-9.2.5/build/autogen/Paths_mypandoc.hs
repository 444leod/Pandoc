{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_mypandoc (
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
bindir     = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/bin"
libdir     = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/lib/x86_64-linux-ghc-9.2.5/mypandoc-0.1.0.0-7j9pjwSqAEA8TKFWalGc3S"
dynlibdir  = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/share/x86_64-linux-ghc-9.2.5/mypandoc-0.1.0.0"
libexecdir = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/libexec/x86_64-linux-ghc-9.2.5/mypandoc-0.1.0.0"
sysconfdir = "/home/elliotalladaye/TEK_2/FUNCTIONNAL/Pandoc/bonus/.stack-work/install/x86_64-linux/cc9380896509e297285695b8ffc2749ce32148dd374a71a488ba58787d5fdc46/9.2.5/etc"

getBinDir     = catchIO (getEnv "mypandoc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "mypandoc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "mypandoc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "mypandoc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mypandoc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mypandoc_sysconfdir") (\_ -> return sysconfdir)




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
