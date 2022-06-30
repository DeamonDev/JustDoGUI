{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module MainMenuUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import View (mainMenuOps)

-- styling
titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (titleAttr,            fg V.cyan)
    ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
      B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
      C.center $ vBox $ renderList' currentIndex mainMenuOps
    ui = vBox [box, renderBottomBar]

renderList' :: Int -> [String] -> [Widget ()]
renderList' k = map str

renderBottomBar :: Widget ()
renderBottomBar = str "[q] quit [h] help"

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey V.KEsc [])) = halt appState