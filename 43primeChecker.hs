

primes :: [Int]
primes = sieve [2..1000]

maxN = length primes

sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (firstElement : rest ) = firstElement : sieve ( filteredRest )
    where filteredRest = filter (not . (== 0 ) . (`mod` firstElement))  rest



isPrime :: Int -> Either String Bool
isPrime n
   | n < 2 = Left "Numbers less than 2 are not candidates for primes"
   | n > maxN = Left "Value exceeds limits of prime checker"
   | otherwise = Right (n `elem` primes)