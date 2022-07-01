module TodosUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

-- styling

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [str "TODOS"]

-- events
handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue showMainMenu
handleEvent appState e = continue appState