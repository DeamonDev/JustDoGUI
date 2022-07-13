{-# LANGUAGE TemplateHaskell #-}

module TodoInfo where

import qualified Data.Text as T
import qualified Brick.Widgets.Border as B

import Control.Lens
import Brick.Forms
import Brick


data TodoInfo = TodoInfo
  { _desc :: T.Text
  }
  deriving (Show)

$(makeLenses ''TodoInfo)

mkForm :: TodoInfo -> Form TodoInfo e ()
mkForm = newForm [B.borderWithLabel (str "label") @@= editTextField desc () Nothing]

