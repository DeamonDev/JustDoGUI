{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NewTodoUI where

import AppState
import Brick
import Brick.Forms (Form, editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Types
import Control.Lens
import Control.Lens.Combinators (makeLenses)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH
import View

-- data structures

-- styling

-- rendering

draw :: AppState -> [Widget ()]
draw appState@NewTodoView {_form = form} = [renderForm $ form]

-- events
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showTodos appState
handleEvent appState@NewTodoView {_todoList = todos, _form = form} e =
  do
    s' <- handleFormEvent e form
    continue NewTodoView {_todoList = todos, _form = s'}
handleEvent appState e = continue appState