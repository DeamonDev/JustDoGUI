{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import TodoItem (TodosList)
import Control.Lens
import Brick.Forms (Form, newForm, editTextField)
import qualified Data.Text as T
import qualified Control.Lens as Lens.Micro.TH

data TodoInfo = TodoInfo
  { _desc :: T.Text
  }
  deriving (Show)

$(makeLenses ''TodoInfo)

getDescription :: TodoInfo -> T.Text 
getDescription TodoInfo {_desc = desc} = desc

initialTodoInfo :: TodoInfo
initialTodoInfo = TodoInfo {_desc = ""}

mkForm :: TodoInfo -> Form TodoInfo e ()
mkForm = newForm [editTextField desc () Nothing]

data View
  = MainMenuView {_currentId :: Int, _menuOptions :: [String], _todoList :: TodosList }
  | HelpView {_todoList :: TodosList }
  | TodoListView {_currentId :: Int, _todoList :: TodosList }
  | NewTodoView {_todoList :: TodosList, _form :: Form TodoInfo () ()}

$(makeLenses ''View)

-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Options", "Quit"]

-- Todo List View