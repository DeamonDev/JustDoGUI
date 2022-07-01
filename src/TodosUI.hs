{-# LANGUAGE OverloadedStrings #-}

module TodosUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens
import qualified Graphics.Vty as V
import ListRender
import TodoItem

-- styling
titleAttr :: A.AttrName
titleAttr = "title"

selectedAttr :: A.AttrName
selectedAttr = "selected"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (titleAttr, fg V.cyan),
    (selectedAttr, V.white `on` V.blue)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    todos = map (^. title) $ getTodos appState
    renderedList = if null todos then [str "Not todos yet"] else ListRender.render currentIndex selectedAttr todos
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
          C.center $ vBox renderedList
    ui = vBox [box, renderBottomBar currentIndex]

renderBottomBar :: Int -> Widget ()
renderBottomBar id = str $ "[+] add todo [-] remove todo [r] return to main menu " ++ show id

-- events
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showMainMenu appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar '+') [])) = continue $ addTodo appState "X"
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState

handleEvent appState e = continue appState