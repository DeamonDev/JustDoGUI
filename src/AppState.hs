{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AppState where

import DbConnection
import Database.SQLite.Simple (Connection)

data AppState = MainMenu { _currentId :: Int, _conn :: Connection }
              | HelpMenu { _currentId :: Int, _conn :: Connection }

init :: Connection -> AppState
init = MainMenu 0
              
getCurrentId :: AppState -> Int 
getCurrentId MainMenu { _currentId = idx } = idx 
getCurrentId HelpMenu { _currentId = idx } = idx

addOne :: AppState -> AppState
addOne (MainMenu k conn) =
  let index = (k - 1) `mod` 4
   in MainMenu index conn

minusOne :: AppState -> AppState
minusOne (MainMenu k conn) =
  let index = (k + 1) `mod` 4
   in MainMenu index conn