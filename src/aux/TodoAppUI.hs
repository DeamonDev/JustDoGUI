{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module TodoAppUI where
import AppState (AppState, initialAppState, addOne, minusOne)
import Brick (BrickEvent (VtyEvent), EventM, Next, Widget, AttrMap, attrMap, App, neverShowCursor, str, fg)
import MenuItem (MenuItem(MainMenuView), MenuItem(TodoListView), mainMenuOps, getCurrentId)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap as A
import Brick.Main
import qualified Graphics.Vty as Brick.Main
import Control.Monad (void)
import Brick.Widgets.Core
import TodoItem

app :: App AppState () ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent 
          , appStartEvent = return 
          , appAttrMap = const theMap
          }

main :: IO () 
main = void $ Brick.Main.defaultMain app AppState.initialAppState 


handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState@(_, MainMenuView _) e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = handleMainMenuEvent appState e 
handleEvent appState@(_, MainMenuView _) e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = handleMainMenuEvent appState e 

handleMainMenuEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleMainMenuEvent appState@(_, MainMenuView _) e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleMainMenuEvent appState@(_, MainMenuView _) e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState


handleTodoListEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleTodoListEvent = undefined


drawUI :: AppState -> [Widget ()]
drawUI appState@(_, MainMenuView _) = drawMainMenuUI appState
drawUI appState@(_, TodoListView _ []) = drawTodoListUI appState

-- main menu UI

drawMainMenuUI :: AppState -> [Widget ()]
drawMainMenuUI appState = [ui] 
              where
                label = str "Just do!"
                currentItem = getCurrentId  $ snd appState
                box = setAvailableSize (150, 150) $
                      B.borderWithLabel label $
                      hLimit 50 $
                      vLimit 15 $ 
                      renderMainMenuOptions currentItem
                ui = C.vCenter $ vBox [ C.hCenter box
                                      , str " "
                                      , C.hCenter $ str "[q] quit "
                                      , C.hCenter $ str $ show currentItem
                                      ]

renderMainMenuOptions :: Int -> Widget () 
renderMainMenuOptions currentItemId = vBox $ map (C.hCenter . str) mainMenuOps

renderMainMenuOptionsAux :: Int -> String -> Widget ()
renderMainMenuOptionsAux id s = undefined

selectedAttrName :: A.AttrName 
selectedAttrName = "selectedListElement"

mainMenuAttrMap :: A.AttrMap 
mainMenuAttrMap = A.attrMap V.defAttr 
            [ (selectedAttrName, fg V.cyan)
            
            ] 
  
-- todoListUI

drawTodoListUI :: AppState -> [Widget ()]
drawTodoListUI appState = undefined 

theMap :: AttrMap 
theMap = attrMap V.defAttr []