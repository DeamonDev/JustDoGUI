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
import View

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
    renderedList = ListRender.renderList currentIndex selectedAttr mainMenuOps
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
          C.center $ vBox renderedList
    ui = vBox [box, renderBottomBar currentIndex]


renderBottomBar :: Int -> Widget ()
renderBottomBar id = str $ "[esc/q] quit [h] help [j] down [k] up " ++ show id


-- events
getBehaviour :: Int -> AppState -> EventM () (Next AppState)
getBehaviour id appState = 
  case id of 
    0 -> continue appState
    1 -> continue appState 
    2 -> continue appState 
    3 -> halt appState

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ showHelp appState
handleEvent appState e@(VtyEvent (V.EvKey V.KEsc [])) = halt appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'q') [])) = halt appState
handleEvent appState@MainMenuView { _currentId = currentId } e@(VtyEvent (V.EvKey V.KEnter [])) = getBehaviour currentId appState
handleEvent appState e = continue appState