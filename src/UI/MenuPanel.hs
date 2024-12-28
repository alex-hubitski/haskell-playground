{-# LANGUAGE OverloadedStrings #-}
module UI.MenuPanel
    ( drawMenuPanel
    ) where

import Brick
import qualified Types as T (AppState(..))
import UI.Types (Name)
import Lens.Micro ((^.))
import qualified Brick.Main as M

drawMenuPanel :: T.AppState -> Widget Name
drawMenuPanel s = Widget Fixed Fixed $ do
    ctx <- getContext
    let windowHeight' = ctx^.availHeightL
    render $
        if windowHeight' > 10
        then drawFullMenuPanel s
        else drawCompactMenuPanel s

drawFullMenuPanel :: T.AppState -> Widget Name
drawFullMenuPanel s = vBox
    [ padLeft (Pad 1) $ str "Controls:"
    , padLeft (Pad 2) $ str "↑/↓ - Scroll vertically"
    , padLeft (Pad 2) $ str "←/→ - Scroll horizontally"
    , padLeft (Pad 2) $ if T.showingDetails s 
                        then str "ESC/Enter - Back to list"
                        else str "Enter - Show details | ESC - Quit"
    ]

drawCompactMenuPanel :: T.AppState -> Widget Name
drawCompactMenuPanel s = vBox
    [ padLeft (Pad 2) $ str "ESC - Back/Exit | F1 - Help"
    ]
