{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser 
  ( parseVideo
  , Thumbnail(..)  -- Export Thumbnail type
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)  -- Add this import
import GHC.Generics
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Types (Video(..))

data Thumbnail = Thumbnail
  { url :: String
  , height :: Int
  , width :: Int
  } deriving (Show, Generic)

instance FromJSON Thumbnail

parseVideo :: Value -> Parser Video
parseVideo = withObject "Video" $ \v -> do
  thumbs <- v .: "thumbnails"
  playlistIdx <- v .: "playlist_index" :: Parser Int
  let lastThumb = listToMaybe (reverse thumbs)
  Video
    <$> (T.pack <$> v .: "id")
    <*> (T.pack <$> v .: "title")
    <*> pure (T.pack $ "#" ++ show playlistIdx)
    <*> (T.pack <$> v .: "description")
    <*> v .: "duration"
    <*> v .: "view_count"
    <*> (T.pack <$> v .: "duration_string")
    <*> pure (T.pack $ maybe "" url lastThumb)
    <*> (T.pack <$> v .: "webpage_url")
    <*> (T.pack <$> v .: "playlist_title")
