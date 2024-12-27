{-# LANGUAGE OverloadedStrings #-}
module YT 
  ( fetchVideoMetadata
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, FromJSON(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))

import Types (Video(..))
import Parser (parseVideo)

instance FromJSON Video where
  parseJSON = parseVideo

fetchVideoMetadata :: String -> Int -> Int -> IO (Either SomeException [Video])
fetchVideoMetadata videoUrl playlistStart count = try $ do
  (exitCode, output, errors) <- readProcessWithExitCode "yt-dlp" args ""
  
  putStrLn "\n=== Raw yt-dlp output ==="
  putStrLn output
  putStrLn "=== End raw output ===\n"
  
  case exitCode of
    ExitSuccess -> 
      let jsonLines = BL.split 10 . BL.fromStrict . TE.encodeUtf8 . T.pack $ output
          videos = mapMaybe decode jsonLines
      in return videos
    ExitFailure code -> 
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors
  
  where
    args = [ "--flat-playlist"
          , "--dump-json"
          , "--playlist-start", show playlistStart
          , "--playlist-end", show (playlistStart + count - 1)
          , videoUrl
          ]