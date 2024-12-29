{-# LANGUAGE OverloadedStrings #-}
module UI.DetailsView 
  ( drawVideoDetails
  , handleDetailsView
  ) where

import Brick
  ( Widget
  , EventM
  , BrickEvent(..)
  , txt
  , hBox
  , vBox
  , withAttr, viewport, ViewportType (Vertical)
  )
import Brick.Widgets.Core (str)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Control.Monad.State (modify)
import qualified Data.Text as T

import Types
import UI.Types

label :: T.Text -> Widget Name
label l = withAttr labelAttr $ txt l

subtitlesWidget :: AppState -> Widget Name
subtitlesWidget state =
  let content = case subtitles state of
          Just subs -> 
              let wrappedLines = map (str . T.unpack) (T.lines subs)  -- Convert T.Text to String
              in vBox wrappedLines  -- Use vBox to stack lines vertically
          Nothing -> 
              str "No subtitles available"  -- Placeholder for no subtitles
  in hBox [label "Subs: ", content]
 
drawVideoDetails :: AppState -> Widget Name
drawVideoDetails s = 
  let selectedVideo = (Vec.!) (videos s) (selected s)
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
      --, hBox [label "Thumbnail: ", txt $ thumbnailUrl selectedVideo]
      , hBox [label "Playlist: ", txt $ playlist_title selectedVideo]
      , subtitlesWidget s
      ]

handleDetailsView :: BrickEvent Name () -> AppState -> EventM Name AppState ()
handleDetailsView e _ = case e of
    VtyEvent (V.EvKey V.KUp [])    -> M.vScrollBy (M.viewportScroll DetailsViewport) (-1)
    VtyEvent (V.EvKey V.KDown [])   -> M.vScrollBy (M.viewportScroll DetailsViewport) 1
    VtyEvent (V.EvKey V.KLeft [])   -> M.hScrollBy (M.viewportScroll DetailsViewport) (-1)
    VtyEvent (V.EvKey V.KRight [])  -> M.hScrollBy (M.viewportScroll DetailsViewport) 1
    VtyEvent (V.EvKey V.KEsc [])    -> modify (\s -> s { showingDetails = False })
    VtyEvent (V.EvKey V.KEnter [])  -> modify (\s -> s { showingDetails = False })
    _                               -> return ()
