{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module JustDo where

import AppName (Name)
import AppState
import Brick
import Control.Lens
import Control.Monad
import Database.SQLite.Simple
import DbConnection (DbConnection (getAllTodos))
import qualified Graphics.Vty as V
import qualified HelpMenuUI
import qualified MainMenuUI
import qualified TodoListUI
import TodoItem

drawUI :: AppState -> [Widget ()]
drawUI appState = case appState of
  MainMenu {} -> MainMenuUI.draw appState
  HelpMenu {} -> HelpMenuUI.draw appState
  TodoList {} -> TodoListUI.draw appState

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState = case appState of
  MainMenu {} -> MainMenuUI.handleEvent appState
  HelpMenu {} -> HelpMenuUI.handleEvent appState
  TodoList {} -> TodoListUI.handleEvent appState

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App AppState () ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  conn <- open "todos.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todo_items (id INTEGER PRIMARY KEY, description TEXT, done INTEGER);"
  _ <- defaultMain app (AppState.init conn)
  putStrLn "Thank you for using Just do!"