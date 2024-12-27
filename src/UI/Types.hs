{-# LANGUAGE OverloadedStrings #-}
module UI.Types 
  ( Name(..)
  , selectedAttr
  , labelAttr
  ) where

import Brick.AttrMap (attrName, AttrName)
import qualified Graphics.Vty as V
import Brick.Util (fg)

-- Name for our viewport
data Name = VideoViewport 
          | DetailsViewport
          deriving (Ord, Show, Eq)

-- Add custom attributes
selectedAttr :: AttrName
selectedAttr = attrName "selected"

labelAttr :: AttrName
labelAttr = attrName "label"
