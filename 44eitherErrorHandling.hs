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