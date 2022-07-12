{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
module AppState where

import DbConnection
import Database.SQLite.Simple (Connection)
import TodoItem
import Control.Lens

data AppState = MainMenu { _currentId :: Int, _todos :: [TodoItem] }
              | HelpMenu { _currentId :: Int, _todos :: [TodoItem] }
              | TodoList { _currentId :: Int, _todos :: [TodoItem] }

$(makeLenses ''AppState)

init :: [TodoItem] -> AppState
init = MainMenu 0

retrieveTodosFromState' :: AppState -> [TodoItem]
retrieveTodosFromState' appState = appState ^. todos

retrieveTodosFromState :: AppState -> [TodoItem]
retrieveTodosFromState MainMenu { _todos = todos } = todos 
retrieveTodosFromState HelpMenu { _todos = todos} = todos 
retrieveTodosFromState TodoList { _todos = todos } = todos
              
getCurrentId :: AppState -> Int 
getCurrentId MainMenu { _currentId = idx } = idx 
getCurrentId HelpMenu { _currentId = idx } = idx


addOne :: AppState -> AppState
addOne (MainMenu k todos) =
  let index = (k - 1) `mod` 4
   in MainMenu index todos

minusOne :: AppState -> AppState
minusOne (MainMenu k todos) =
  let index = (k + 1) `mod` 4
   in MainMenu index todos

showMainMenu :: AppState -> AppState 
showMainMenu HelpMenu { _currentId = idx, _todos = todos } = MainMenu idx todos

showHelpMenu :: AppState -> AppState 
showHelpMenu MainMenu { _currentId = idx, _todos = todos } = HelpMenu idx todos