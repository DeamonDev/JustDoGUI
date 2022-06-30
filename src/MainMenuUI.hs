module MainMenuUI where
import Brick
import AppState


draw :: AppState -> [Widget ()]
draw appState = [str "MainMenuUI"]