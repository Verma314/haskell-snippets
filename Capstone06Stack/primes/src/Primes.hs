module Primes where

primes :: [Int]
primes = sieve [2..10000]


sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (firstElement : rest ) = firstElement : sieve ( filteredRest )
    where filteredRest = filter (not . (== 0 ) . (`mod` firstElement))  rest



isPrime :: Int -> Maybe Bool
isPrime n   | n < 2 = Nothing
            | n > (length primes) = Nothing
            | otherwise = Just ( n `elem` primes)




unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 primes = []
unsafePrimeFactors n [] = []
unsafePrimeFactors number primes = filter ((== 0) . (mod number)) primes


-- making it safe:
primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n > length (primes) = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter ( < n ) primes