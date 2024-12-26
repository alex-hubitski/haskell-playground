{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YT 
  ( fetchVideoMetadata
  , VideoInfo(..)
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))

import Types (Video(..))

data VideoInfo = VideoInfo
  { _type :: String
  , id :: String
  , title :: String
  , playlist_index :: Int
  } deriving (Show, Generic)

instance FromJSON VideoInfo where
  parseJSON = withObject "VideoInfo" $ \v -> VideoInfo
    <$> v .: "_type"
    <*> v .: "id"
    <*> v .: "title"
    <*> v .: "playlist_index"

fetchVideoMetadata :: String -> Int -> Int -> IO (Either SomeException [Video])
fetchVideoMetadata videoUrl playlistStart count = try $ do
  (exitCode, output, errors) <- readProcessWithExitCode "yt-dlp" args ""
  
  case exitCode of
    ExitSuccess -> 
      let jsonLines = BL.split 10 . BL.fromStrict . TE.encodeUtf8 . T.pack $ output
          decodedVideos = mapMaybe decode jsonLines
          videos = map toVideo decodedVideos
      in do
        putStrLn $ "Number of JSON lines: " ++ show (length jsonLines)
        putStrLn $ "Number of decoded videos: " ++ show (length decodedVideos)
        putStrLn $ "Final number of videos: " ++ show (length videos)
        return videos
    ExitFailure code -> 
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors
  
  where
    args = [ "--flat-playlist"
          , "--dump-json"
          , "--playlist-start", show playlistStart
          , "--playlist-end", show (playlistStart + count - 1)
          , videoUrl
          ]
    
    toVideo :: VideoInfo -> Video
    toVideo info = Video
      { videoId = T.pack $ YT.id info
      , videoTitle = T.pack $ title info
      , index = T.pack $ "#" ++ show (playlist_index info)
      } 