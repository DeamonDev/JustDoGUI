{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TodoListUI where

import AppName (Name)
import AppState
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens
import Data.Map
import Database.SQLite.Simple
import qualified Graphics.Vty as V
import ListRender
import TodoItem

-- data

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
    todosListIds = Prelude.map (^. TodoItem.id) (appState ^. todos)
    todosListTitles = Prelude.map (^. title) (appState ^. todos)
    todosList = zipWith (\i title -> "[" ++ show i ++ "] " ++ title) todosListIds todosListTitles
    listToRender = Prelude.map (\x -> (x, Nothing)) todosList :: [(String, Maybe [AttrName])]
    mainMenuList = toList $ updateMainMenuOps currentIndex (fromList listToRender)
    renderedList =
      if Prelude.null todosListIds
        then [str "Not todos yet"]
        else ListRender.render' mainMenuList
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr titleAttr $ str "Just do!") $
          C.center $ vBox renderedList
    ui = vBox [box, renderBottomBar currentIndex]

renderBottomBar :: Int -> Widget ()
renderBottomBar id = str $ "[r] return to main menu  [+] add todo  [-] remove todo " ++ show id

-- events handling
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showMainMenu appState
handleEvent appState e = continue appState