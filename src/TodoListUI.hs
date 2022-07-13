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
import qualified Data.Map as M
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
    todosListIds = map (^. TodoItem.id) (appState ^. todos)
    todosListTitles = map (^. title) (appState ^. todos)
    todosListDones = map (^. done) (appState ^. todos)
    doneAttrList = map (\d -> if not d then Nothing else Just [doneAttr]) todosListDones
    todosList = zipWith (\i title -> "[" ++ show i ++ "] " ++ title) todosListIds todosListTitles
  in zip todosList doneAttrList


updateMainMenuOps :: Int -> M.Map String (Maybe [AttrName]) -> M.Map String (Maybe [AttrName])
updateMainMenuOps currentIndex m =
  let key = M.keys m !! currentIndex
      oldValue = m M.! key
      newValue = case oldValue of
        Nothing -> Just [selectedAttr]
        Just l ->
          if doneAttr `elem` l then Just (selectedDoneAttr : l) else Just (selectedAttr : l)
   in M.insert key newValue m

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
    mainMenuList = M.toList $ updateMainMenuOps currentIndex (M.fromList listToRender)
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
renderBottomBar id = str $ "[r] return to main menu  [+] add todo  [-] remove todo  [d/u] mark as done/undone" ++ show id

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