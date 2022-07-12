{-# language OverloadedStrings #-}
module ScottyHello where

import qualified Web.Scotty as S 

main :: IO ()
main = do 
  S.scotty 3000 myApp

myApp :: S.ScottyM () 
myApp = do
  S.get "/" $ 
    S.text "not yet implemented"
  S.get "/post/:id" $ 
    error "not yet implemented"