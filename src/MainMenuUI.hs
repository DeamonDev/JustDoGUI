module MainMenuUI where
import Brick
import AppState


draw :: AppState -> [Widget ()]
draw appState = [str "MainMenuUI"]

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState = undefined