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
  , ViewportType(Both)
  , fg
  )
import Brick.Main ()
import qualified Graphics.Vty as V
import Control.Monad.State 
  ( modify
  , get
  , liftIO
  )
import Control.Concurrent.Async (async, link, wait)
import Brick.BChan (BChan, writeBChan)
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Border (hBorder, borderWithLabel)
import qualified Data.Text as T
import Types (AppState(..), Video(..))
import UI.Types
import UI.ListView
import UI.DetailsView
import UI.MenuPanel (drawMenuPanel, resetScroll)
import qualified YT

app :: BChan AppState -> App AppState AppState Name
app _ = App
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

handleEvent :: BrickEvent Name AppState -> EventM Name AppState ()
handleEvent (AppEvent newState) = modify (\_ -> newState)
handleEvent _e@(VtyEvent (V.EvResize _ newHeight)) = do
    liftIO $ putStrLn ("New window height: " ++ show newHeight)
    modify (\st -> st { windowHeight = newHeight })
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    currentState <- get
    if not (showingDetails currentState)
    then do
        let selectedVideo = (Vec.!) (videos currentState) (selected currentState)
        -- Show details immediately with loading state
        modify (\s -> s { subtitles = Just (Left "Loading subtitles..."), showingDetails = True })
        resetScroll
        
        -- Start async fetch without waiting
        st <- get
        let eventChan' = eventChan st
        liftIO $ do
            asyncFetch <- async $ YT.fetchSubtitles (T.unpack $ webpage_url selectedVideo)
            link asyncFetch
            -- Handle result in background and send update through event channel
            _ <- async $ do
                result <- wait asyncFetch
                case result of
                    Left err -> do
                        putStrLn $ "Error fetching subtitles: " ++ show err
                        writeBChan eventChan' $ st { subtitles = Just (Right "Subtitles unavailable") }
                    Right subs -> do
                        writeBChan eventChan' $ st { subtitles = Just (Right subs) }
            return ()
    else modify (\s -> s { showingDetails = False })
handleEvent e@(VtyEvent _) = do
    st <- get
    if showingDetails st
    then handleDetailsView e st
    else handleListView e st
handleEvent _ = return ()
