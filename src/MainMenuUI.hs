{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MainMenuUI where
import Brick
import AppState
import qualified Graphics.Vty as V 
import qualified Brick.Widgets.Center as C
import View (mainMenuOps)



draw :: AppState -> [Widget ()]
draw appState = [ui]
            where 
              ui = C.vCenter $ vBox $ map str mainMenuOps

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ minusOne appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ addOne appState
handleEvent appState e@(VtyEvent (V.EvKey V.KEsc []))        = halt appState