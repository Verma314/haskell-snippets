-- lazy i/o

--not working
main :: IO ()
main = do
       inputList <- getContents
       let reversed =  reverse inputList
       putStrLn reversed

-- not working
reverser :: IO ()
reverser = do
   input <- getContents
   print input

   
main2 :: IO ()
main2 = do
       inputList <- getContents
       mapM_ print inputList  