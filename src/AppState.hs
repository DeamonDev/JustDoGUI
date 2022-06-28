module AppState where
import TodoItem (TodosList)
import MenuItem (MenuItem (MainMenuView))

type AppState = (TodosList, MenuItem)

initialAppState :: AppState 
initialAppState = ([], MenuItem.MainMenuView 0)

addOne :: AppState -> AppState 
addOne appState@(x, MainMenuView k) = (x, MainMenuView (k + 1))

minusOne :: AppState -> AppState 
minusOne appState@(x, MainMenuView k) = (x, MainMenuView (k - 1))