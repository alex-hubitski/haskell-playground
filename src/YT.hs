{-# LANGUAGE OverloadedStrings #-}
module YT 
  ( fetchVideoMetadata
  , fetchSubtitles
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
  --putStrLn output
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

fetchSubtitles :: String -> IO (Either SomeException T.Text)
fetchSubtitles videoUrl = try $ do
  let args = [ "--skip-download"
             , "--write-sub"
             , "--write-auto-sub"
             , "--sub-lang", "en"
             , "--sub-format", "best"
             , "--convert-subs", "srt"
             , "-o", "temp_subtitle"
             , videoUrl
             ]
  (exitCode, _, errors) <- readProcessWithExitCode "yt-dlp" args ""
  
  case exitCode of
    ExitSuccess -> do
      (catExitCode, output, catErrors) <- readProcessWithExitCode "cat" ["temp_subtitle.en.srt"] ""
      _ <- readProcessWithExitCode "rm" ["temp_subtitle.en.srt"] ""
      case catExitCode of
        ExitSuccess -> return $ formatSubtitles $ T.pack output
        ExitFailure catCode -> 
          error $ "cat failed with code " ++ show catCode ++ ": " ++ catErrors
    ExitFailure code -> 
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors

-- Function to format subtitles into multiple lines
formatSubtitles :: T.Text -> T.Text
formatSubtitles = T.unlines . map T.strip . T.lines