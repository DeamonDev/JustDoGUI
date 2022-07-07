{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View where

import Brick
import Brick.Forms (Form, editTextField, newForm, (@@=))
import qualified Brick.Widgets.Border as B
import Control.Lens
import qualified Control.Lens as Lens.Micro.TH
import qualified Data.Text as T
import TodoItem (TodosList)

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
mkForm = newForm [B.borderWithLabel (str "label") @@= editTextField desc () Nothing]

data View
  = MainMenuView {_currentId :: Int, _menuOptions :: [String], _todoList :: TodosList}
  | HelpView {_todoList :: TodosList}
  | TodoListView {_currentId :: Int, _todoList :: TodosList}
  | NewTodoView {_todoList :: TodosList, _form :: Form TodoInfo () ()}

$(makeLenses ''View)

-- Main Menu View

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Options", "Quit"]

-- Todo List View