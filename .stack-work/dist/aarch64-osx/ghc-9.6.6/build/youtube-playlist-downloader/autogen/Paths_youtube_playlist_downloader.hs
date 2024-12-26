{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_youtube_playlist_downloader (
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
bindir     = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/bin"
libdir     = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/lib/aarch64-osx-ghc-9.6.6/youtube-playlist-downloader-0.1.0.0-4fKCCFz4fQn3QmQqdXhaMT-youtube-playlist-downloader"
dynlibdir  = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/share/aarch64-osx-ghc-9.6.6/youtube-playlist-downloader-0.1.0.0"
libexecdir = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/libexec/aarch64-osx-ghc-9.6.6/youtube-playlist-downloader-0.1.0.0"
sysconfdir = "/Users/alexhubitski/Samples/playground3/.stack-work/install/aarch64-osx/893982f8eff697a057d741f00c38c1de5414eb713c524fd8e3a1dc9dded79712/9.6.6/etc"

getBinDir     = catchIO (getEnv "youtube_playlist_downloader_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "youtube_playlist_downloader_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "youtube_playlist_downloader_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "youtube_playlist_downloader_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "youtube_playlist_downloader_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "youtube_playlist_downloader_sysconfdir") (\_ -> return sysconfdir)



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
