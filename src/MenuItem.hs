{-# LANGUAGE NamedFieldPuns #-}
module MenuItem where

-- simple enumeration
data MenuItem = MainMenuView { _currentId :: Int } | TodoListView { _currentId :: Int } | HabitsTrackerView { _currentId :: Int }

getCurrentId :: MenuItem -> Int
getCurrentId MainMenuView { _currentId } = _currentId
getCurrentId TodoListView { _currentId } = _currentId
getCurrentId HabitsTrackerView { _currentId } = _currentId

type Operation = String

mainMenuViewOps :: [Operation]
mainMenuViewOps = ["[q] quit"]

mainMenuOps :: [String]
mainMenuOps = ["Todos", "Habits", "Very long nonsense text", "Help"]

todoListViewOps :: [Operation]
todoListViewOps = ["[q] quit", "[+] add todo", "[-] remove todo"]
