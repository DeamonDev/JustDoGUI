{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
module AppState where

import DbConnection
import Database.SQLite.Simple (Connection)
import TodoItem
import Control.Lens

data AppState = MainMenu { _currentId :: Int, _conn :: Connection }
              | HelpMenu { _currentId :: Int, _conn :: Connection }
              | TodoList { _currentId :: Int, _conn :: Connection, _todos :: [TodoItem] }

$(makeLenses ''AppState)

init :: Connection -> AppState
init = MainMenu 0

getCurrentId :: AppState -> Int
getCurrentId MainMenu { _currentId = idx } = idx
getCurrentId HelpMenu { _currentId = idx } = idx
getCurrentId TodoList { _currentId = idx } = idx


addOne :: AppState -> AppState
addOne (MainMenu k conn) =
  let index = (k - 1) `mod` 4
   in MainMenu index conn
addOne (TodoList k conn todos) = 
  let index = (k - 1) `mod` (length todos)
  in TodoList index conn todos 

minusOne :: AppState -> AppState
minusOne (MainMenu k todos) =
  let index = (k + 1) `mod` 4
   in MainMenu index todos
minusOne (TodoList k conn todos) = 
  let index = (k + 1) `mod` (length todos)
  in TodoList index conn todos 


showMainMenu :: AppState -> AppState
showMainMenu HelpMenu { _currentId = idx, _conn = conn } = MainMenu idx conn
showMainMenu TodoList { _conn = conn } = MainMenu 0 conn 

showHelpMenu :: AppState -> AppState
showHelpMenu MainMenu { _currentId = idx, _conn = conn } = HelpMenu idx conn

showTodosList :: Connection -> [TodoItem] -> AppState
showTodosList = TodoList 0


