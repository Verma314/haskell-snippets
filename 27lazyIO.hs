-- lazy i/o


main3 :: IO ()
main3 = do
       inputList <- getContents
       let reversed =  reverse inputList
       putStrLn reversed

reverser :: IO ()
reverser = do
   input <- getContents
   print input

   
main2 :: IO ()
main2 = do
       inputList <- getContents
       mapM_ print inputList  



-- after we have an input, we can split it with 'lines'

toInts :: String -> [Int]
toInts = map read  . lines

{-
> toInts "123\n234\n34\n"
[123,234,34]
-}

-- we can hook toInts with an IO function,
main4 :: IO ()
main4 = do
       contents <- getContents
       let numbers = toInts contents
       print (sum numbers)





quoteGenerator  :: Int -> String
quoteGenerator 1 = "Hi QUote 1"
quoteGenerator 2 = "Hi QUote 2"
quoteGenerator 3 = "Hi QUote 3"
quoteGenerator _ = "Hi QUote Random"

main5 :: IO ()
main5 = do
       putStrLn "Enter a number"
       contents <- getContents
       mapM_ putStrLn (map quoteGenerator (toInts contents))
-- LAZY IO




-- how to actively keep summing up inputs?

-- bad logic, not working:
addCumulativeUpTo :: Int -> [Int] -> Int
addCumulativeUpTo n listOfInts = sum(take n listOfInts)

addIndexToList :: [Int] -> [(Int,Int)]
addIndexToList list = zip list [1..(length list)]

sumValuesUpToIndex :: [Int] -> [Int]
sumValuesUpToIndex list = map (\(n,y) -> (addCumulativeUpTo n list))  (addIndexToList list )


main :: IO ()
main = do
       contents <- getContents
       mapM_ putStrLn (map show (map sumValuesUpToIndex [toInts contents]))



