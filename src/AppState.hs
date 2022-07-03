{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AppState where

import Brick.Forms
import Control.Lens
import TodoItem
import View
import Prelude hiding (id)

-- later, we'll expand it to (View, DbConnection, ...)
type AppState = View

initialAppState :: AppState
initialAppState = MainMenuView 0 mainMenuOps []

getCurrentId :: AppState -> Int
getCurrentId MainMenuView {_currentId = currentId} = currentId
getCurrentId TodoListView {_currentId = currentId} = currentId

getTodos :: AppState -> [TodoItem]
getTodos TodoListView {_todoList = todos} = todos
getTodos MainMenuView {_todoList = todos} = todos
getTodos HelpView {_todoList = todos} = todos
getTodos NewTodoView {_todoList = todos} = todos

addOne :: AppState -> AppState
addOne (MainMenuView k l t) =
  let index = (k - 1) `mod` length l
   in MainMenuView index l t
addOne (TodoListView k t) =
  let index = (k - 1) `mod` length t
   in TodoListView index t

minusOne :: AppState -> AppState
minusOne (MainMenuView k l t) =
  let index = (k + 1) `mod` length l
   in MainMenuView index l t
minusOne (TodoListView k t) =
  let index = (k + 1) `mod` length t
   in TodoListView index t

getCurrentTodoIndex :: AppState -> Int
getCurrentTodoIndex (TodoListView k t) = if null t then 0 else maximum (map (^. id) t) + 1

--enterAction :: AppState -> AppState

showHelp :: AppState -> AppState
showHelp appState =
  let todos = getTodos appState
   in HelpView todos

showMainMenu :: AppState -> AppState
showMainMenu appState =
  let todos = getTodos appState
   in MainMenuView 0 mainMenuOps todos

showTodos :: AppState -> AppState
showTodos appState =
  let todos = getTodos appState
   in TodoListView 0 todos

showNewTodoWindow :: AppState -> AppState
showNewTodoWindow appState =
  let todos = getTodos appState
   in NewTodoView todos $ mkForm initialTodoInfo

removeCurrentlySelectedTodo :: AppState -> AppState
removeCurrentlySelectedTodo TodoListView {_currentId = k, _todoList = t} =
   TodoListView {_currentId = k - 1, _todoList = removeAt k t}

removeAt :: Int -> [a] -> [a]
removeAt idx xs = lft ++ rgt
  where
    (lft, _ : rgt) = splitAt idx xs

completeCurrentlySelectedTodo :: AppState -> AppState
completeCurrentlySelectedTodo TodoListView {_currentId = id, _todoList = t} =
  TodoListView{_currentId = id, _todoList = modifyAt id (`changeTitle` "[+] ") t}

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt idx f xs = lft ++ [f x] ++ rgt
  where
    (lft, x : rgt) = splitAt idx xs