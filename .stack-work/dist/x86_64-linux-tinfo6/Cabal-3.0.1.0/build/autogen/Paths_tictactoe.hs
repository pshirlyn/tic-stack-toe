{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tictactoe (
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

bindir     = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/bin"
libdir     = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/lib/x86_64-linux-ghc-8.8.3/tictactoe-0.1.0.0-8m4MgLtaevvEJShJpoDe0U"
dynlibdir  = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/share/x86_64-linux-ghc-8.8.3/tictactoe-0.1.0.0"
libexecdir = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/libexec/x86_64-linux-ghc-8.8.3/tictactoe-0.1.0.0"
sysconfdir = "/home/shirlyn/code/ticstacktoe/.stack-work/install/x86_64-linux-tinfo6/211597e03301217f084271752bd9bb1c6b0b6c19a182a230e88a9be2fceff328/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tictactoe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tictactoe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tictactoe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tictactoe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tictactoe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tictactoe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
