{-# LANGUAGE NamedFieldPuns #-}

module View where

import TodoItem (TodosList)

data View
  = MainMenuView {_currentId :: Int, menuOptions :: [String]}
  | HelpView 
  | TodoListView {_currentId :: Int, todoList :: TodosList}


-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Very long nonsense text", "quit"]

-- Todo List View