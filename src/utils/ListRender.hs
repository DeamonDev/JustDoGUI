{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ListRender where

import Brick
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import AppName (Name)

-- rendering
render' :: [(String, Maybe [A.AttrName])] -> [Widget ()]
render' l = listRenderAux' 0 l []
  where
    listRenderAux' index l acc
      | index >= length l = acc
      | otherwise =
        let
          maybeAttrList = snd (l !! index)
          plainWidget = str (fst $ l !! index)
          widget = case maybeAttrList of 
            Nothing -> plainWidget
            Just attrList -> applyStyles attrList plainWidget
          newAcc = acc ++ [widget]
         in listRenderAux' (index + 1) l newAcc

applyStyles :: [A.AttrName] -> Widget () -> Widget ()
applyStyles [] w = w
applyStyles [a] w = withAttr a w 
applyStyles (x:xs) w = applyStyles xs $ withAttr x w


renderWithRightPadding' :: [(String, Maybe [A.AttrName])] -> [Widget ()]
renderWithRightPadding' l = map (padRight Max) $ render' l 