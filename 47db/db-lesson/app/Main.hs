module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

addUser :: String -> IO ()
addUser username =  do  
                    conn <- open "tools.db"
                    execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
                    print "user added"
                    close conn



main :: IO ()
main = putStrLn "hi"