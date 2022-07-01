{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AppState where

import View

-- later, we'll expand it to (View, DbConnection, ...)
type AppState = View

initialAppState :: AppState
initialAppState = MainMenuView 0 mainMenuOps

getCurrentId :: AppState -> Int
getCurrentId MainMenuView {_currentId = currentId} = currentId
getCurrentId TodoListView {_currentId = currentId} = currentId

addOne :: AppState -> AppState
addOne (MainMenuView k l) =
  let index = (k - 1) `mod` length l
   in MainMenuView index l

minusOne :: AppState -> AppState
minusOne (MainMenuView k l) =
  let index = (k + 1) `mod` length l
   in MainMenuView index l

--enterAction :: AppState -> AppState 

showHelp :: AppState -> AppState 
showHelp _ = HelpView 

showMainMenu :: AppState 
showMainMenu = MainMenuView 0 mainMenuOps

showTodos :: AppState 
showTodos = TodoListView 0 [] False 