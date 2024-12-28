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
  , liftIO
  )
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Border (hBorder, borderWithLabel)
import qualified Data.Text as T  -- Import Data.Text as T

import Types
import UI.Types
import UI.ListView
import UI.DetailsView
import UI.MenuPanel (drawMenuPanel, resetScroll)
import qualified YT

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
    liftIO $ putStrLn ("New window height: " ++ show newHeight)
    modify (\st -> st { windowHeight = newHeight })
handleEvent e@(VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    if not (showingDetails st)
    then do
        let selectedVideo = (Vec.!) (videos st) (selected st)
        result <- liftIO $ YT.fetchSubtitles (T.unpack $ webpage_url selectedVideo)
        case result of
            Left err -> liftIO $ putStrLn $ "Error fetching subtitles: " ++ show err
            Right subs -> do
                modify (\s -> s { subtitles = Just subs, showingDetails = True })
                -- Reset the scroll position of the details viewport
                resetScroll
    else modify (\s -> s { showingDetails = False })
handleEvent e@(VtyEvent _) = do
    st <- get
    if showingDetails st
    then handleDetailsView e st
    else handleListView e st
handleEvent _ = return ()