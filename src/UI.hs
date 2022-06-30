module UI where

import AppState (AppState, initialAppState)
import Brick
import Control.Monad (void)
import View 
import MainMenuUI

app :: App AppState () ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = undefined
          , appStartEvent = return
          , appAttrMap = undefined
          }

main :: IO ()
main = void $ defaultMain  app initialAppState


drawUI :: AppState -> [Widget ()]
drawUI appState = case appState of 
   MainMenuView { } -> MainMenuUI.draw appState
   TodoListView { } -> undefined 
