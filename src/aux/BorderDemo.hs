{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module BorderDemo where



import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Util (fg, on, bg)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget, Padding (Max)
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str, padBottom, vBox, padLeft, padRight, padLeftRight
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick (Padding(Pad))
import qualified Control.Arrow as C
import qualified Data.IntMap.Lazy as C
import Brick (padTop)
import Graphics.Vty.Attributes.Color (red)
import Graphics.Vty (blue)
import Graphics.Vty (cyan)

styles :: [(T.Text, BS.BorderStyle)]
styles =
    [ ("ascii", BS.ascii)
    , ("unicode", BS.unicode)
    , ("unicode bold", BS.unicodeBold)
    , ("unicode rounded", BS.unicodeRounded)
    , ("custom", custom)
    , ("from 'x'", BS.borderStyleFromChar 'x')
    ]

custom :: BS.BorderStyle
custom =
    BS.BorderStyle { BS.bsCornerTL = '/'
                   , BS.bsCornerTR = '\\'
                   , BS.bsCornerBR = '/'
                   , BS.bsCornerBL = '\\'
                   , BS.bsIntersectFull = '.'
                   , BS.bsIntersectL = '.'
                   , BS.bsIntersectR = '.'
                   , BS.bsIntersectT = '.'
                   , BS.bsIntersectB = '.'
                   , BS.bsHorizontal = '*'
                   , BS.bsVertical = '!'
                   }

borderDemos :: [Widget ()]
borderDemos = map mkBorderDemo styles

mkBorderDemo :: (T.Text, BS.BorderStyle) -> Widget ()
mkBorderDemo (styleName, sty) =
    withBorderStyle sty $
    B.borderWithLabel (str "label") $
    vLimit 10 $
    C.vCenter $
    txt $ "  " <> styleName <> " style  "

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (titleAttr,            fg V.cyan)
    ]

ops :: [String]
ops = ["[+] add todo ", "[-] remove todo ", "[q] quit"]

colorDemo :: Widget ()
colorDemo =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    B.borderWithLabel (withAttr titleAttr $ str "operations") $
    vLimit 1 $
    hBox $
    str <$> ops

colorAttrName :: A.AttrName 
colorAttrName = "currentlySelectedColor"

colorAttr :: V.Attr 
colorAttr = bg red 

colorMappings :: [(A.AttrName, V.Attr)]
colorMappings = [ (colorAttrName, colorAttr) ]

currentlySelected :: Widget ()
currentlySelected = 
  updateAttrMap (A.applyAttrMappings colorMappings) $
  padRight Max $
  withAttr colorAttrName $ str "Todo3"


ui :: Widget ()
ui =
    B.hBorderWithLabel (str "Just DoIt")
    <=> padBottom  Max (vBox [str "TODO", str "TODO2", currentlySelected])
    <=> updateAttrMap (A.applyAttrMappings borderMappings) (B.hBorderWithLabel (withAttr titleAttr $ str "operations"))
    <=> hBox (str <$> ops)

main :: IO ()
main = M.simpleMain ui
