{-# LANGUAGE OverloadedStrings #-}

module HelpUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import Brick.Types
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

-- styling
helpAttr :: A.AttrName
helpAttr = "help"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (helpAttr, fg V.cyan)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]  
  where
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr helpAttr $ str "help") $
          C.center $ vBox $ [str "We always wanted to create..."]
    ui = vBox [box, bottomBar]


bottomBar :: Widget () 
bottomBar = str "[r] return to main menu"

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showMainMenu appState