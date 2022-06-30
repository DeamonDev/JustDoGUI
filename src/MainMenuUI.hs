{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MainMenuUI where
import Brick
import AppState
import qualified Graphics.Vty as V



draw :: AppState -> [Widget ()]
draw appState = [str "MainMenuUI"]

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState