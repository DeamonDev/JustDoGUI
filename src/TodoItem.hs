{-# LANGUAGE TemplateHaskell #-}

module TodoItem where

import Control.Lens

data TodoItem = TodoItem
  { _id :: Int,
    _title :: String,
    _done :: Bool
  }
  deriving (Show)

type TodosList = [TodoItem]

$(makeLenses ''TodoItem)

markAsDone :: TodoItem -> TodoItem
markAsDone todo = if todo ^. done then todo else set done True todo

markAsUndone :: TodoItem -> TodoItem
markAsUndone todo = if todo ^. done then set done False todo else todo

changeTitle :: TodoItem -> String -> TodoItem 
changeTitle todo@TodoItem{_id = id, _title = t, _done = d} newTitle = TodoItem{_id = id, _title = newTitle ++ t, _done = d}
