{-# LANGUAGE OverloadedStrings #-}
module UI.ListView 
  ( drawVideo
  , handleListView
  , scrollIfNeeded
  ) where

import Brick
  ( Widget
  , EventM
  , BrickEvent(..)
  , padLeft
  , txt
  , str
  , hBox
  , withAttr
  , Padding(..)
  )
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Control.Monad.State (modify)
import Control.Monad (when)
import qualified Data.Vector as Vec
import Lens.Micro ((^.))

import Types
import UI.Types

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

handleListView :: BrickEvent Name AppState -> AppState -> EventM Name AppState ()
handleListView e st = case e of
    VtyEvent (V.EvKey V.KUp []) -> do
        let newPos = max 0 (selected st - 1)
        when (newPos /= selected st) $ do
            scrollIfNeeded VideoViewport newPos
            modify (\s -> s { selected = newPos })

    VtyEvent (V.EvKey V.KDown []) -> do
        let maxPos = Vec.length (videos st) - 1
            newPos = min maxPos (selected st + 1)
        when (newPos /= selected st) $ do
            scrollIfNeeded VideoViewport newPos
            modify (\s -> s { selected = newPos })

    VtyEvent (V.EvKey V.KRight []) -> 
        M.hScrollBy (M.viewportScroll VideoViewport) 20
    VtyEvent (V.EvKey V.KLeft []) -> 
        M.hScrollBy (M.viewportScroll VideoViewport) (-20)
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
