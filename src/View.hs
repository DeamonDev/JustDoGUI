{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module View where

import TodoItem (TodosList)
import Control.Lens

data View
  = MainMenuView {_currentId :: Int, _menuOptions :: [String], _todoList :: TodosList }
  | HelpView {_todoList :: TodosList }
  | TodoListView {_currentId :: Int, _todoList :: TodosList }
  | NewTodoView {_todoList :: TodosList}

$(makeLenses ''View)

-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Options", "Quit"]

-- Todo List View