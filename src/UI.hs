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
  )
import qualified Brick.Main as M
import Control.Monad.State (put, get, modify)
import Control.Monad (when, unless)
import Brick.Widgets.Border (hBorder, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core 
  ( vBox
  , padLeft
  , txt
  , Padding(..)
  , hBox
  , str 
  , withAttr
  )
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Brick.Types (ViewportType(Both), Location(..), Viewport)
import qualified Brick.Widgets.Center as C
import Brick.AttrMap (attrName, AttrName)
import Brick.Util (fg)
import qualified Brick.Types as T
import Lens.Micro ((^.))
import Lens.Micro ((^.), _1, _2)

import Types

-- Name for our viewport
data Name = VideoViewport 
          | DetailsViewport
          deriving (Ord, Show, Eq)

-- Add custom attributes
selectedAttr :: AttrName
selectedAttr = attrName "selected"

labelAttr :: AttrName
labelAttr = attrName "label"

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

-- Use M.ViewportScroll from Brick.Main
videoScroll :: M.ViewportScroll Name
videoScroll = M.viewportScroll VideoViewport

detailsScroll :: M.ViewportScroll Name
detailsScroll = M.viewportScroll DetailsViewport

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
      , drawMenu s
      ]

drawVideoDetails :: AppState -> Widget Name
drawVideoDetails s = 
  let selectedVideo = (Vec.!) (videos s) (selected s)
      label l = withAttr labelAttr $ txt l
  in vBox
      [ txt ""  -- Remove the "Video Details:" line since it's now in the border
      , hBox [label "Title: ", txt $ videoTitle selectedVideo]
      , hBox [label "ID: ", txt $ videoId selectedVideo]
      , hBox [label "Index: ", txt $ index selectedVideo]
      , txt ""
      , hBox [label "Duration: ", txt $ durationString selectedVideo]
      , hBox [label "Views: ", txt $ T.pack (show $ viewCount selectedVideo)]
      , txt ""
      , label "Description:"
      , txt $ description selectedVideo
      , txt ""
      , hBox [label "URL: ", txt $ webpage_url selectedVideo]
      , hBox [label "Thumbnail: ", txt $ thumbnailUrl selectedVideo]
      , hBox [label "Playlist: ", txt $ playlist_title selectedVideo]
      ]

drawVideo :: Int -> Int -> Video -> Widget Name
drawVideo selectedIdx idx v =
  let cursor = if idx == selectedIdx then ">" else " "
      content = hBox
        [ str cursor
        , txt $ "  " <> index v <> "  "
        , txt $ "[" <> videoId v <> "]  "
        , txt $ videoTitle v
        ]
  in padLeft (Pad 1) $
     (if idx == selectedIdx then withAttr selectedAttr else id) $
     content

drawMenu :: AppState -> Widget Name
drawMenu s = vBox
  [ padLeft (Pad 1) $ str "Controls:"
  , padLeft (Pad 2) $ str "↑/↓ - Scroll vertically"
  , padLeft (Pad 2) $ str "←/→ - Scroll horizontally"
  , padLeft (Pad 2) $ if showingDetails s 
                      then str "ESC/Enter - Back to list"
                      else str "Enter - Show details | ESC - Quit"
  ]

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent e@(VtyEvent _) = do
    st <- get
    if showingDetails st
    then handleDetailsView e st
    else handleListView e st

handleDetailsView :: BrickEvent Name () -> AppState -> EventM Name AppState ()
handleDetailsView e st = case e of
    VtyEvent (V.EvKey V.KUp [])    -> M.vScrollBy detailsScroll (-1)
    VtyEvent (V.EvKey V.KDown [])   -> M.vScrollBy detailsScroll 1
    VtyEvent (V.EvKey V.KLeft [])   -> M.hScrollBy detailsScroll (-1)
    VtyEvent (V.EvKey V.KRight [])  -> M.hScrollBy detailsScroll 1
    VtyEvent (V.EvKey V.KEsc [])    -> modify (\s -> s { showingDetails = False })
    VtyEvent (V.EvKey V.KEnter [])  -> modify (\s -> s { showingDetails = False })
    _                               -> return ()

handleListView :: BrickEvent Name () -> AppState -> EventM Name AppState ()
handleListView e st = case e of
    VtyEvent (V.EvKey V.KUp []) -> do
        let newPos = max 0 (selected st - 1)
        when (newPos /= selected st) $ do
            scrollIfNeeded VideoViewport newPos  -- Pass viewport name directly
            modify (\s -> s { selected = newPos })

    VtyEvent (V.EvKey V.KDown []) -> do
        let maxPos = Vec.length (videos st) - 1
            newPos = min maxPos (selected st + 1)
        when (newPos /= selected st) $ do
            scrollIfNeeded VideoViewport newPos  -- Pass viewport name directly
            modify (\s -> s { selected = newPos })

    VtyEvent (V.EvKey V.KRight []) -> 
        M.hScrollBy videoScroll 20
    VtyEvent (V.EvKey V.KLeft []) -> 
        M.hScrollBy videoScroll (-20)
    VtyEvent (V.EvKey V.KEnter []) -> do
        -- Reset viewport scroll before showing details
        M.vScrollToBeginning (M.viewportScroll DetailsViewport)
        M.hScrollToBeginning (M.viewportScroll DetailsViewport)
        modify (\s -> s { showingDetails = True })
    VtyEvent (V.EvKey V.KEsc []) ->
        M.halt
    _ -> return ()

-- Change the type signature to use Name instead of ViewportScroll
scrollIfNeeded :: Name -> Int -> EventM Name AppState ()
scrollIfNeeded vpName pos = do
    viewport <- M.lookupViewport vpName
    case viewport of
        Nothing -> return ()
        Just vp' -> do
            let (_, h) = vp'^.T.vpSize
                visibleItems = h - 2
                currentScroll = vp'^.T.vpTop
                -- Calculate scroll amount based on position
                scrollAmount = if pos < currentScroll 
                             then negate $ currentScroll - pos
                             else if pos >= currentScroll + visibleItems
                                  then pos - (currentScroll + visibleItems) + 1
                                  else 0
            when (scrollAmount /= 0) $
                M.vScrollBy (M.viewportScroll vpName) scrollAmount