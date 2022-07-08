{-# LANGUAGE OverloadedStrings #-}

module JustDo where

import Database.SQLite.Simple

main :: IO () 
main = do
  conn <- open "todos.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todo_items (id INTEGER PRIMARY KEY, description TEXT, done INTEGER);"
  putStrLn "JustDo v.0.1"