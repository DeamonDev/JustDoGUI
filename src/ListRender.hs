{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ListRender where

import Brick
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

-- rendering
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