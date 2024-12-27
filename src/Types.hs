{-# LANGUAGE DeriveGeneric #-}
module Types 
  ( Video(..)
  , AppState(..)
  , initialState
  ) where

import qualified Data.Text as T
import qualified Data.Vector as V

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

data AppState = AppState
  { videos :: V.Vector Video
  , selected :: Int
  , showingDetails :: Bool  -- New field
  } deriving (Show, Eq)

initialState :: AppState
initialState = AppState
  { videos = V.empty
  , selected = 0
  , showingDetails = False
  }