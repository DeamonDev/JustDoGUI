module AppState where
import TodoItem (TodosList, TodoItem (TodoItem))
import MenuItem (MenuItem (MainMenuView), mainMenuOps)

type AppState = ([MenuItem], MenuItem)

initialAppState :: AppState 
initialAppState = ([MainMenuView 0, MainMenuView 1, MainMenuView 2], MenuItem.MainMenuView 0)

addOne :: AppState -> AppState 
addOne appState@(x, MainMenuView k) = (x, MainMenuView $ (k + 1) `mod` length x)

minusOne :: AppState -> AppState 
minusOne appState@(x, MainMenuView k) = (x, MainMenuView $ (k - 1) `mod` length x)