{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HelpMenuUI where

import AppState
import Brick
import qualified Brick.AttrMap as A
import Brick.Types
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Control.Lens
import qualified DbConnection
import Control.Monad.IO.Class
import DbConnection
import Control.Monad.Trans.Class

helpAttr :: A.AttrName
helpAttr = "help"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (helpAttr, fg V.cyan)
  ]

-- rendering
draw :: AppState -> [Widget ()]
draw appState = [ui]
  where
    box =
      updateAttrMap (A.applyAttrMappings borderMappings) $
        B.borderWithLabel (withAttr helpAttr $ str "help") $
          C.center $
            vBox $
              [ str
                  "We always wanted to create the best TODO app in the observable universe. \
                  \ \nFinally, we ended up watching Seinfield's TV series."
              ]
    ui = vBox [box, bottomBar]

bottomBar :: Widget ()
bottomBar = str "[r] return to main menu"

handleEvent :: AppState -> BrickEvent () () -> EventM () (Next AppState)
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ showMainMenu appState
handleEvent appState e@(VtyEvent (V.EvKey (V.KChar 'a') [])) = do
                                                            let conn_ = appState ^. conn 
                                                            liftIO $ insertTodo conn_ "X"
                                                            continue appState
handleEvent appState e = continue appState