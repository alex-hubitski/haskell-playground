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
import Control.Monad.State (put, get)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Core 
  ( vBox
  , padLeft
  , txt
  , Padding(..)
  , hBox
  , str
  , visible
  , hLimit
  , vLimit
  )
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Brick.Types (ViewportType(Vertical, Both))
import qualified Brick.Widgets.Center as C

import Types

-- Name for our viewport
data Name = VideoViewport
  deriving (Ord, Show, Eq)

app :: App AppState () Name
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

-- Use M.ViewportScroll from Brick.Main
videoScroll :: M.ViewportScroll Name
videoScroll = M.viewportScroll VideoViewport

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = C.center $ vBox
      [ hBorder
      , viewport VideoViewport Both $
          vBox $ zipWith (drawVideo (selected s)) [0..] (Vec.toList $ videos s)
      , hBorder
      , drawMenu
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
     (if idx == selectedIdx then visible else id) $
     content

drawMenu :: Widget Name
drawMenu = vBox
  [ padLeft (Pad 1) $ str "Controls:"
  , padLeft (Pad 2) $ str "↑/↓ - Move selection"
  , padLeft (Pad 2) $ str "←/→ - Scroll horizontally"
  , padLeft (Pad 2) $ str "ESC - Quit"
  ]

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KUp []))   = do
    s <- get
    let newPos = max 0 (selected s - 1)
    put s { selected = newPos }
    M.vScrollBy videoScroll (-1)

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    s <- get
    let newPos = min (Vec.length (videos s) - 1) (selected s + 1)
    put s { selected = newPos }
    M.vScrollBy videoScroll 1

handleEvent (VtyEvent (V.EvKey V.KRight [])) = 
    M.hScrollBy videoScroll 20  -- Increased scroll amount

handleEvent (VtyEvent (V.EvKey V.KLeft [])) = 
    M.hScrollBy videoScroll (-20)  -- Increased scroll amount

handleEvent (VtyEvent (V.EvKey V.KEsc []))  = M.halt
handleEvent _                               = return () 