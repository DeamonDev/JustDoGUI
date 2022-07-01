{-# LANGUAGE NamedFieldPuns #-}

module View where

import TodoItem (TodosList)

data View
  = MainMenuView {_currentId :: Int, _menuOptions :: [String]}
  | HelpView
  | TodoListView {_currentId :: Int, _todoList :: TodosList }

-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Options", "Quit"]

-- Todo List View