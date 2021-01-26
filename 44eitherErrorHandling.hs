import qualified Data.Char as D
-- error handline with Either



primes :: [Int]
primes = sieve [2..1000]

maxN = length primes

sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (firstElement : rest ) = firstElement : sieve ( filteredRest )
    where filteredRest = filter (not . (== 0 ) . (`mod` firstElement))  rest



-- defining our custom error class,

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge = "Value exceeded max bound"
    show InvalidValue = "Value is not a valid candidate for primality checking"



isPrime :: Int -> Either PrimeError Bool
isPrime n
   | n < 2 = Left InvalidValue
   | n > maxN = Left TooLarge
   | otherwise = Right (n `elem` primes)


-- making it more userfriendly by returning a String

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "Number is a prime"
displayResult (Right False) = "Number is a composite"
displayResult (Left primeError) = show primeError -- we can take Eithers out of ctx

-- let us design an IO action to complete the primality testing software,

main :: IO ()
main = do   
        input <- getLine
        let inputInt = read input :: Int -- or use, input <- read <$> getLine
        let primeTestResults =  (displayResult . isPrime) inputInt
        putStrLn primeTestResults
        main


-----------------------------------------        
-----------------------------------------              
-------------- Exercises ----------------
-----------------------------------------        
-----------------------------------------

{-
Make a function addStrInts that takes two Ints 
represented as Strings and adds them. 
The function would return an Either String Int.
 The Right constructor should return the result, 
 provided that the two arguments can be parsed into Ints (use Data.Char isDigit to check). 
 Return a different Left result for the three possible cases:

First value can’t be parsed.
Second value can’t be parsed.
Neither value can be parsed.

-}

isNegative :: String -> Bool
isNegative (x:num) = if ( x == '-') then True
                     else False

-- 

isStrNumber :: String -> Bool
isStrNumber "" = False
isStrNumber givenNumberAsString =  if isNegative givenNumberAsString 
                                   then  isStrNumber (tail givenNumberAsString)
                                   else all D.isDigit givenNumberAsString 

strToNum :: String -> Int
strToNum givenNumberAsString = if isNegative givenNumberAsString
                               then (-1) * (read (tail givenNumberAsString))
                               else read givenNumberAsString


addStrInts :: String -> String -> Either String Int
addStrInts input1 input2 | isInput1Num && isInput2Num = Right (input1Parsed + input2Parsed)
                         | (not isInput1Num) && isInput2Num = Left "input 1 is not a number"
                         | isInput1Num && (not isInput2Num) = Left "input 2 is not a number"
                         | (not isInput1Num) && (not isInput2Num) = Left "Neither are numbers"
    where isInput1Num = isStrNumber input1
          isInput2Num = isStrNumber input2
          input1Parsed = strToNum input1
          input2Parsed = strToNum input2





