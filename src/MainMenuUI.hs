{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MainMenuUI where

import AppName (Name)
import AppState
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Data.Map
import qualified Graphics.Vty as V
import ListRender

-- data
mainMenuOps :: Map String (Maybe [AttrName])
mainMenuOps = fromList [("[1] Todos", Nothing), ("[2] Habits", Nothing), ("[3] Options", Nothing), ("[4] Quit", Nothing)]

updateMainMenuOps :: Int -> Map String (Maybe [AttrName]) -> Map String (Maybe [AttrName])
updateMainMenuOps currentIndex m =
  let key = keys m !! currentIndex
      oldValue = m ! key
      newValue = case oldValue of
        Nothing -> Just [selectedAttr]
        Just l -> Just (l ++ [selectedAttr])
   in insert key newValue m

-- styling
titleAttr :: A.AttrName
titleAttr = "title"

selectedAttr :: A.AttrName
selectedAttr = "selected"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (titleAttr, fg V.cyan),
    (selectedAttr, V.black `on` V.yellow)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    mainMenuList = toList $ updateMainMenuOps currentIndex mainMenuOps
    renderedList =
      ListRender.render' mainMenuList
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
          C.center $ vBox renderedList
    ui = vBox [box, renderBottomBar currentIndex]

renderBottomBar :: Int -> Widget ()
renderBottomBar id = str $ "[esc|q] quit  [h] help  [j] down  [k] up" ++ show id

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'q') [])) = halt appState
handleEvent appState e = continue appState