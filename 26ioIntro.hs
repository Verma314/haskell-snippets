import System.Environment
import Control.Monad
-- I/O

hello :: String -> String
hello name = "Hello " ++ name


main2 :: IO()
main2 = do
       putStrLn "hi"
       name <- getLine --  <- is used because getLine returns an IO String , but we want to use name like a regular String
       let statement = hello name -- <- you have to use let whenever we create variables that AREN'T IO type
       putStrLn (hello name)



test :: IO()
test = do 
       putStrLn "hii"
       randomDoubleValue <- getLine -- AGAIN, <- used because it retuns an IO String, which we want to pretend is a normal string
       let myDoubleVal = read randomDoubleValue -- <- using let because myDoubleVal etc are not IO types
       let mySum = myDoubleVal + 1.1
       let statement = show mySum
       putStrLn statement


{-
Q21.2
Create a program that asks the user to input a number and then returns the nth Fibonacci numbers (see lesson 8 for an example of computing Fibonacci numbers).
-}       

fastFib a _ 0 = a 
fastFib a b counter = fastFib (a+b) a (counter - 1)

computeFib :: IO ()
computeFib = do
             putStrLn "Enter n"
             number <- getLine
             let getNthFibonacci = fastFib 1 1 (read number)
             putStrLn (show getNthFibonacci)
             computeFib  -- method is calling itself





--------------------------------------------------------------
-- accepting n number of inputs

-- to test in the OS terminal change the name to 'main'
main3 :: IO ()
main3 = do
        args <- getArgs
        putStrLn "hi"
        mapM_ putStrLn args

-- to be able to use getArgs
-- we need to compile it in the command line        
-- ghc 26ioIntro.hs
-- ./26ioIntro.hs 1 2 3
-- this outputs:
{-
hi
1
2
3
-}


{-Write a main that uses mapM to call getLine three times, 
and then use mapM_ to print out the values’ input.
(Hint: You’ll need to throw away an argument when using mapM with getLine; use (\_ -> ...) to achieve this.)-}

--   number <- getLine
main4 :: IO()
main4 = do 
       myMap <- mapM (\ _ ->  getLine) [1,2,3]
       mapM_ putStrLn myMap


-- we need to write a program that lets us sum n number of command line arguments,
-- this n is accepted dynamically from the prompt
-- then n values are read from the CLI
-- the sum of the n values is returned
main :: IO()
main = do   
       args <- getArgs
       let linesToRead = if length args > 0
                   then read (head args)
                   else 0
       numbers <- myReplicateM linesToRead getLine -- always remember, all the IO functions use <-, let is for normies
       let sumOfInputs = sum (map read numbers :: [Int])
       print (sumOfInputs)




{-Q1:
Write your own version of replicateM, myReplicateM, that uses mapM. (Don’t worry too much about the type signature.)-}


myReplicateM n ioAction = mapM (\ _ -> ioAction) [1..n]
