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
import qualified DbConnection
import Control.Monad.IO.Class

-- data

prepareListToRender :: AppState -> [(String, Maybe [AttrName])]
prepareListToRender appState = 
  let 
    currentIndex = getCurrentId appState
    todosListIds = Prelude.map (^. TodoItem.id) (appState ^. todos)
    todosListTitles = Prelude.map (^. title) (appState ^. todos)
    todosListDones = Prelude.map (^. done) (appState ^. todos)
    doneAttrList = Prelude.map (\d -> if not d then Nothing else Just [doneAttr]) todosListDones
    todosList = zipWith (\i title -> "[" ++ show i ++ "] " ++ title) todosListIds todosListTitles
  in zip todosList doneAttrList


updateMainMenuOps :: Int -> Map String (Maybe [AttrName]) -> Map String (Maybe [AttrName])
updateMainMenuOps currentIndex m =
  let key = keys m !! currentIndex
      oldValue = m ! key
      newValue = case oldValue of
        Nothing -> Just [selectedAttr]
        Just l ->
          if doneAttr `elem` l then Just (selectedDoneAttr : l) else Just (selectedAttr : l)
   in insert key newValue m

-- styling
doneAttr :: A.AttrName 
doneAttr = "done"

titleAttr :: A.AttrName
titleAttr = "title"

selectedAttr :: A.AttrName
selectedAttr = "selected"

selectedDoneAttr :: A.AttrName
selectedDoneAttr = "selectedDoneAttr"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (titleAttr, fg V.cyan),
    (doneAttr, fg V.green),
    (selectedAttr, V.black `on` V.yellow),
    (selectedDoneAttr, V.green `on` V.yellow)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    currentIndex = getCurrentId appState
    listToRender = prepareListToRender appState
    mainMenuList = toList $ updateMainMenuOps currentIndex (fromList listToRender)
    renderedList =
      if Prelude.null listToRender
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
handleEvent appState@TodoList{_conn = conn, _todos = todos} e@(VtyEvent (V.EvKey (V.KChar 'd') [])) = do
                                                            let currId = getCurrentId appState
                                                            liftIO $ DbConnection.markAsDone conn (currId + 1)
                                                            newTodos <- liftIO $ DbConnection.getAllTodos conn
                                                            continue appState{_todos = newTodos}
handleEvent appState@TodoList{_conn = conn, _todos = todos} e@(VtyEvent (V.EvKey (V.KChar 'u') [])) = do
                                                            let currId = getCurrentId appState
                                                            liftIO $ DbConnection.markAsUndone conn (currId + 1)
                                                            newTodos <- liftIO $ DbConnection.getAllTodos conn
                                                            continue appState{_todos = newTodos}
handleEvent appState e = continue appState