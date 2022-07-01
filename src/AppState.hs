{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AppState where

import TodoItem
import View

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

addTodo :: AppState -> String -> AppState
addTodo (TodoListView k t) s =
  let newTodo = TodoItem 10 s False
   in TodoListView k (newTodo : t)

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