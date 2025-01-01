module Types
  ( Video(..)
  , AppState(..)
  , initialState
  , parseVideo
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Vector as V
import Brick.BChan (BChan)
import Data.Maybe (listToMaybe)
import Parser (url)

-- Add parseVideo as an alias for the FromJSON instance
parseVideo :: Value -> Parser Video
parseVideo = parseJSON

data Video = Video
  { videoId :: T.Text
  , videoTitle :: T.Text
  , index :: T.Text
  , description :: T.Text
  , duration :: Double
  , viewCount :: Integer
  , durationString :: T.Text
  , thumbnailUrl :: T.Text
  , webpage_url :: T.Text
  , playlist_title :: T.Text
  } deriving (Show, Eq)

instance FromJSON Video where
  parseJSON = withObject "Video" $ \v -> do
    thumbs <- v .: fromString "thumbnails"
    playlistIdx <- v .: fromString "playlist_index" :: Parser Int
    let lastThumb = listToMaybe (reverse thumbs)
    Video
      <$> (T.pack <$> v .: fromString "id")
      <*> (T.pack <$> v .: fromString "title")
      <*> pure (T.pack $ "#" ++ show playlistIdx)
      <*> (T.pack <$> v .: fromString "description")
      <*> v .: fromString "duration"
      <*> v .: fromString "view_count"
      <*> (T.pack <$> v .: fromString "duration_string")
      <*> pure (T.pack $ maybe "" url lastThumb)
      <*> (T.pack <$> v .: fromString "webpage_url")
      <*> (T.pack <$> v .: fromString "playlist_title")

data AppState = AppState
  { eventChan :: BChan AppState
  , videos :: V.Vector Video
  , selected :: Int
  , showingDetails :: Bool
  , windowHeight :: Int
  , subtitles :: Maybe (Either T.Text T.Text)  -- Left = loading, Right = actual subtitles
  }

instance Show AppState where
  show s = "AppState {videos = " ++ show (videos s) 
         ++ ", selected = " ++ show (selected s)
         ++ ", showingDetails = " ++ show (showingDetails s)
         ++ ", windowHeight = " ++ show (windowHeight s)
         ++ ", subtitles = " ++ show (subtitles s) ++ "}"

instance Eq AppState where
  a == b = videos a == videos b
        && selected a == selected b
        && showingDetails a == showingDetails b
        && windowHeight a == windowHeight b
        && subtitles a == subtitles b

initialState :: BChan AppState -> AppState
initialState chan = AppState
  { eventChan = chan
  , videos = V.empty
  , selected = 0
  , showingDetails = False
  , windowHeight = 0
  , subtitles = Nothing
  }
