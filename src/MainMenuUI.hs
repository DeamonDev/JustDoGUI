{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MainMenuUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import ListRender
import View (mainMenuOps)

-- styling
titleAttr :: A.AttrName
titleAttr = "title"

selectedAttr :: A.AttrName 
selectedAttr = "selected"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (titleAttr, fg V.cyan)
  , (selectedAttr,   V.white `on` V.blue)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
          C.center $ vBox $ ListRender.renderList currentIndex selectedAttr mainMenuOps
    ui = vBox [box, renderBottomBar currentIndex]

renderList' :: Int -> [String] -> [Widget ()]
renderList' k = map str

renderBottomBar :: Int -> Widget ()
renderBottomBar id = str $ "[esc/q] quit [h] help" ++ show id

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ showHelp appState
handleEvent appState e@(VtyEvent (V.EvKey V.KEsc [])) = halt appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'q') [])) = halt appState
handleEvent appState e = continue appState