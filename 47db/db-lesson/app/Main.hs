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

                       
instance FromRow User where
   fromRow = User <$> field
                  <*> field

instance FromRow Tool where
   fromRow = Tool <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field


printUsers :: IO ()
printUsers = do 
        withConn "tools.db" $
                (\ conn ->
                           do
                           values <- query_ conn "SELECT * FROM users"  :: IO [User]    
                           mapM_ print values          
                )            


-- can execute all queries returning a tool
printToolQuery :: Query -> IO ()
printToolQuery query = withConn "tools.db" $
                        ( \ conn -> 
                                    do
                                    values <- query_ conn query :: IO [Tool]
                                    mapM_ print values)
                               
-- to use the above,
printTools :: IO ()
printTools =  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ 
                mconcat [ "select * from tools ", 
                                "where id in ", 
                                "(select tool_id from checkedout);"]                                          

------------------------------------------
------------------------------------------
------------------------------------------
-- Update the DB  (important) ------------


-- first obtain the tool
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
   resp <- query conn
           "SELECT * FROM tools WHERE id = (?)"
           (Only toolId) :: IO [Tool]
   return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x


-- then update the tool (the Haskell type)
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
   { lastReturned = date
   , timesBorrowed = 1 + timesBorrowed tool
   }


-- then put this new tool values in the DB,
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =  withConn "tools.db" $
                            \conn -> do
                              let q = mconcat ["UPDATE TOOLS SET  "
                                              ,"lastReturned = ?,"
                                              ," timesBorrowed = ? "
                                              ,"WHERE ID = ?;"]

                              execute conn q (lastReturned tool
                                             , timesBorrowed tool
                                             , toolId tool)
                              print "tool updated"


---------
-- putting it all together

{-
 updateToolTable, takes a toolId, fetches the current date, 
 and then performs the necessary steps to update the tool in the table.
-}

updateToolTable toolId = do
                         conn <- open "tools.db"
                         chosenTool <- selectTool conn toolId
                         currentDate <-  utctDay <$> getCurrentTime  
                         let newTool = (fmap updateTool chosenTool) <*> (pure currentDate) -- we need a Maybe type for date, don't use Just, use pure to convert

                         --let newTool = updateTool removeMaybeContextTool currentDate
                         -- nowt hat we have our updated tool,
                         -- we can use, updateOrWarn ::  Maybe Tool -> IO ()

                         -- newTool is a MaybeTool, and updateOrWarn takes just that as a parameter
                         updateOrWarn newTool
                         close conn



-------------------------------
-------------------------------
-------------------------------
-- Deleting records -------------
checkin :: Int -> IO ()
checkin toolId =  withConn "tools.db" $
                     \conn -> do
                       execute conn
                         "DELETE FROM checkedout WHERE tool_id = (?);"
                         (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
   checkin toolId
   updateToolTable toolId
-- why? so that it can update that the tool was borrowed, it deletes the tool from the checkout entries   

------------------------------------------
------------------------------------------
------------------------------------------
--- Creating a User Interface of sorts ---




promptAndAddUser :: IO ()
promptAndAddUser = do
   print "Enter new user name"
   userName <- getLine
   addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
   print "Enter the id of the user"
   userId <- pure read <*> getLine
   print "Enter the id of the tool"
   toolId <- pure read <*> getLine
   checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
   print "enter the id of tool"
   toolId <- pure read <*> getLine
   checkinAndUpdate toolId


performCommand :: String -> IO ()
performCommand command
   | command == "users" = printUsers >> main
   | command == "tools" = printTools >> main
   | command == "adduser" = promptAndAddUser >> main
   | command == "checkout" = promptAndCheckout >> main
   | command == "checkin" = promptAndCheckin >> main
   | command == "in" = printAvailable >> main
   | command == "out" = printCheckedout >> main
   | command == "quit" = print "bye!"
   | otherwise = print "Sorry command not found" >> main





main :: IO ()
main = do
   print "Enter a command"
   command <- getLine
   performCommand command