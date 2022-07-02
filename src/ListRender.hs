{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ListRender where

import Brick
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Data.Map

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



render :: Int -> A.AttrName -> [String] -> [Widget ()]
render index selectedAttr l = listRenderAux 0 index l []
  where
    listRenderAux curr index l acc
      | curr >= length l = acc
      | curr == index =
        let newAcc = acc ++ [withAttr selectedAttr $ str (l !! curr)]
         in listRenderAux (curr + 1) index l newAcc
      | curr /= index =
        let newAcc = acc ++ [str (l !! curr)]
         in listRenderAux (curr + 1) index l newAcc


renderWithPadding :: Int -> A.AttrName -> [String] -> [Widget ()]
renderWithPadding index selectedAttr l = listRenderAux 0 index l []
  where
    listRenderAux curr index l acc
      | curr >= length l = acc
      | curr == index =
        let newAcc = acc ++ [padRight Max $ withAttr selectedAttr $ str (l !! curr)]
         in listRenderAux (curr + 1) index l newAcc
      | curr /= index =
        let newAcc = acc ++ [padRight Max $ str (l !! curr)]
         in listRenderAux (curr + 1) index l newAcc