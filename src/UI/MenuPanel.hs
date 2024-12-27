{-# LANGUAGE OverloadedStrings #-}
module UI.MenuPanel
    ( drawMenuPanel
    ) where

import Brick
import Types (AppState(..))
import UI.Types (Name)

drawMenuPanel :: AppState -> Widget Name
drawMenuPanel s = vBox
    [ padLeft (Pad 1) $ str "Controls:"
    , padLeft (Pad 2) $ str "↑/↓ - Scroll vertically"
    , padLeft (Pad 2) $ str "←/→ - Scroll horizontally"
    , padLeft (Pad 2) $ if showingDetails s 
                        then str "ESC/Enter - Back to list"
                        else str "Enter - Show details | ESC - Quit"
    ]
