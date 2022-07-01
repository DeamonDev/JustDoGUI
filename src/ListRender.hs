{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ListRender where
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Brick



-- rendering
renderList :: Int -> A.AttrName -> [String] -> [Widget ()]
renderList index selectedAttr l = listRenderAux 0 index l []
  where
    listRenderAux curr index l acc
      | curr >= length l = acc
      | curr == index =
        let newAcc = acc ++ [withAttr selectedAttr $ str (l !! curr) ]
        in listRenderAux (curr + 1) index l newAcc
      | curr /= index     =
        let newAcc = acc ++ [str (l !! curr)]
        in listRenderAux (curr + 1) index l newAcc