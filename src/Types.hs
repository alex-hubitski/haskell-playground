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
  } deriving (Show, Eq)

data AppState = AppState
  { videos :: V.Vector Video
  , selected :: Int
  } deriving (Show, Eq)

initialState :: AppState
initialState = AppState
  { videos = V.empty
  , selected = 0
  } 