module NewTodoUI where

import AppState
import Brick
import Brick.Types
import qualified Graphics.Vty as V

draw :: AppState -> [Widget ()]
draw appState = [str "NewTodo"]

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showTodos appState
handleEvent appState e = continue appState