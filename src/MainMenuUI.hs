{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}

module MainMenuUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import ListRender
import AppName (Name)
import Data.Map

-- data 

-- TODO change this to [(String, Maybe Attr)] and set to Nothing
mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Options", "Quit"]

mainMenuOps' :: Map String (Maybe [AttrName])
mainMenuOps' = fromList [("Todos", Nothing), ("Habits", Nothing), ("Options", Nothing), ("Quit", Nothing)]

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
    (selectedAttr, V.white `on` V.blue)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    renderedList =
      ListRender.render' $ toList $ updateMainMenuOps currentIndex mainMenuOps'
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