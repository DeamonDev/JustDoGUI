{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module NewTodoUI where

import AppState
import Brick
import Brick.Forms (Form, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Types
import Control.Lens
import Control.Lens.Combinators (makeLenses)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH
import TodoItem
import View
import View (getDescription)

-- data structures

-- styling

-- rendering

draw :: AppState -> [Widget ()]
draw appState@NewTodoView {_form = form} = [renderForm form]

removeQuotes :: String -> String
removeQuotes [] = []
removeQuotes [x] = []
removeQuotes xs = tail (init xs)

-- events
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState@NewTodoView {_todoList = todos, _form = form} e@(VtyEvent (V.EvKey V.KEnter [])) =
  let c = formState form
      newTodo = TodoItem 1 (removeQuotes $ show $ getDescription c) False
   in continue $ showTodos appState {_todoList = todos ++ [newTodo]}
-- very important event
handleEvent appState@NewTodoView {_todoList = todos, _form = form} e =
  do
    s' <- handleFormEvent e form
    continue NewTodoView {_todoList = todos, _form = s'}
-- submit form

handleEvent appState e = continue appState