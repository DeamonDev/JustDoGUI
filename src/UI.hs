module UI where

import AppState (AppState, initialAppState)
import Brick
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified HelpUI
import qualified MainMenuUI
import qualified TodosUI
import View

app :: App AppState () ()
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

main :: IO ()
main = void $ defaultMain app initialAppState >> putStrLn "Thank you for using Just do!"

drawUI :: AppState -> [Widget ()]
drawUI appState = case appState of
  MainMenuView {} -> MainMenuUI.draw appState
  TodoListView {} -> TodosUI.draw appState
  HelpView {}       -> HelpUI.draw appState

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState = case appState of
  MainMenuView {} -> MainMenuUI.handleEvent appState
  TodoListView {} -> TodosUI.handleEvent appState
  HelpView {}        -> HelpUI.handleEvent appState

theMap :: AttrMap
theMap = attrMap V.defAttr []
