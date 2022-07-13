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
import qualified Brick.Widgets.Border as B
import Lens.Micro.TH
import TodoItem
import TodoInfo
import DbConnection
import Control.Monad.IO.Class
-- data structures

-- styling

-- rendering

draw :: AppState -> [Widget ()]
draw appState@NewTodo {_form = form} = [vBox [renderForm form, str "[enter] submit form [esc] abort"]]

removeQuotes :: String -> String
removeQuotes [] = []
removeQuotes [x] = []
removeQuotes xs = tail (Prelude.init xs)

-- events
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
--handleEvent appState e@(VtyEvent (V.EvKey V.KEsc [])) = continue $ showTodosList appState
handleEvent appState@NewTodo { _form = form, _conn = conn } e@(VtyEvent (V.EvKey V.KEnter [])) = do
      let c = formState form
          title = removeQuotes $ show $ c ^. desc 
      liftIO $ insertTodo conn title
      todos <- liftIO $ getAllTodos conn
      continue $ showTodosList conn todos
-- very important event
handleEvent appState@NewTodo { _form = form, _conn = conn} e =
  do
    s' <- handleFormEvent e form
    continue NewTodo {  _form = s', _conn = conn }
-- submit form

handleEvent appState e = continue appState