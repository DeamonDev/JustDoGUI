{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module JustDo where

import AppState 
import Brick
import Database.SQLite.Simple
import qualified Graphics.Vty as V
import qualified MainMenuUI
import AppName (Name)
import Control.Monad
import qualified HelpMenuUI
import DbConnection (DbConnection(getAllTodos))

drawUI :: AppState -> [Widget ()]
drawUI appState = case appState of 
  MainMenu { } -> MainMenuUI.draw appState
  HelpMenu { } -> HelpMenuUI.draw appState 

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState = case appState of 
  MainMenu { } -> MainMenuUI.handleEvent appState
  HelpMenu { } -> HelpMenuUI.handleEvent appState

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
  todos <- getAllTodos conn
  finalAppState <- defaultMain app (AppState.init todos) 
  execute_ conn "DELETE FROM todo_items;"
  -- get todos from finalAppState and insert it into our database
  putStrLn "Thank you for using Just do!"