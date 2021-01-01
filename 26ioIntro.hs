-- I/O

hello :: String -> String
hello name = "Hello " ++ name

main :: IO()
main = do
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


