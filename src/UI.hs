{-# LANGUAGE OverloadedStrings #-}
module UI
  ( drawUI
  , handleEvent
  , app
  ) where

import Brick
  ( App(..)
  , BrickEvent(..)
  , EventM
  , Widget
  , neverShowCursor
  , attrMap
  , viewport
  , vBox
  , padLeft
  , Padding(..)
  , txt
  , withAttr
  , str
  , ViewportType(Both)
  , fg
  )
import qualified Brick.Main as M
import Control.Monad.State 
  ( modify
  , get
  )
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Border (hBorder, borderWithLabel)
import Debug.Trace (trace)

import Types
import UI.Types
import UI.ListView
import UI.DetailsView
import UI.MenuPanel (drawMenuPanel)

app :: App AppState () Name
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap V.defAttr
      [ (selectedAttr, fg V.yellow)
      , (labelAttr, fg V.yellow)
      ]
  , appChooseCursor = neverShowCursor
  }

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = C.center $ vBox
      [ hBorder
      , if showingDetails s
        then borderWithLabel (withAttr labelAttr $ txt "Video Details") $ 
             viewport DetailsViewport Both $ 
             padLeft (Pad 1) $ drawVideoDetails s
        else viewport VideoViewport Both $
             vBox $ zipWith (drawVideo (selected s)) [0..] (Vec.toList $ videos s)
      , hBorder
      , drawMenuPanel s
      ]

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent _e@(VtyEvent (V.EvResize _ newHeight)) = do
    modify (\st -> st { windowHeight = newHeight })
handleEvent e@(VtyEvent _) = do
    st <- get
    if showingDetails st
    then handleDetailsView e st
    else handleListView e st
handleEvent _ = return ()