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
-- takes in the db value , and an IO action
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName operation = do
                            conn <- open dbName
                            operation conn 
                            close conn

addUserOperation ::  String -> Connection -> IO () 
addUserOperation user conn = do
        execute conn "INSERT INTO users (username) VALUES (?)" (Only user)
        putStrLn "user added"       

{-
How to use:
> withConn "tools.db" (addUserOperation "Zezima")
user added


-- here ^ you are creating a partial function, and using complicated operations, let us simplify
-}        

addUserNew :: String -> IO ()
addUserNew user = withConn "tools.db" $
                        (\ conn ->  do 
                                    execute conn  "INSERT INTO users (username) VALUES (?)" (Only user)
                                    putStrLn "added user"
                                    )

-- lambdas are tedious, so we create,
executeWrapper :: ToRow q =>  Query -> q -> String -> (Connection -> IO ())
executeWrapper sqlStml tuples successMsg =
            (\ conn ->  do 
                        execute conn sqlStml tuples
                        putStrLn successMsg
            )
      
-- now,
addUserNew2 :: String -> IO ()
addUserNew2 user = withConn "tools.db" $
                  executeWrapper "INSERT INTO users (username) VALUES (?)" (Only user) "added the user"


checkoutMy :: Int -> Int -> IO ()
checkoutMy userId toolId = withConn "tools.db" $
        executeWrapper  "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" 
                        (userId,toolId) 
                        "Checked out the tool"

                            

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                           execute conn
                             "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                             (userId,toolId)

                          

--------------------------------------------
--------------------------------------------
--------------------------------------------
--------------------------------------------

-- Reading from DB, and
-- Converting rows into Haskell types.


data Tool = Tool
 { toolId :: Int
 , name :: String
 , description :: String
 , lastReturned :: Day
 , timesBorrowed :: Int
 }

 data User = User
 { userId :: Int
 , userName :: String
 }

instance Show User where
   show user = mconcat [ show $ userId user
                       , ".)  "
                       , userName user]

instance Show Tool where
   show tool = mconcat [ show $ toolId tool
                       , ".) "
                       , name tool
                       , "\n description: "
                       , description tool
                       , "\n last returned: "
                       , show $ lastReturned tool
                       , "\n times borrowed: "
                       , show $ timesBorrowed tool
                       , "\n"]

                       















main :: IO ()
main = putStrLn "hi"