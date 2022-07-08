module AppState where

import DbConnection
import Database.SQLite.Simple (Connection)

data AppState = MainMenu { _currentId :: Int, _conn :: Connection }
              | HelpMenu { _currentId :: Int, _conn :: Connection }
              