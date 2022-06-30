{-# LANGUAGE NamedFieldPuns #-}

module View where

import TodoItem (TodosList)

data View
  = MainMenuView {_currentId :: Int, menuOptions :: [String]}
  | TodoListView {_currentId :: Int, todoList :: TodosList}

getCurrentId :: View -> Int
getCurrentId MainMenuView {_currentId = currentId} = currentId
getCurrentId TodoListView {_currentId = currentId} = currentId

-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Very long nonsense text", "quit"]

-- Todo List View