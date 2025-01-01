{-# LANGUAGE DeriveGeneric #-}
module Parser 
  ( Thumbnail(..)  -- Export Thumbnail type
  ) where

import Data.Aeson
import GHC.Generics

data Thumbnail = Thumbnail
  { url :: String
  , height :: Int
  , width :: Int
  } deriving (Show, Generic)

instance FromJSON Thumbnail
