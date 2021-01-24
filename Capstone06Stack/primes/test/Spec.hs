import Test.QuickCheck
import Primes
import Data.Maybe

main :: IO ()
main = do   
     quickCheck prop_validPrimesOnly
     quickCheckWith stdArgs { maxSuccess = 1000} prop_primesArePrimes
     quickCheckWith stdArgs { maxSuccess = 1000} prop_nonPrimesAreComposite
-- prop1. isPrime holds the property that it returns Nothing if we give it a prime > list of primes (in our program)
    -- or, values less than 0
prop_validPrimesOnly val = if val < 0 || val > length Primes.primes
                           then result == Nothing
                           else isJust ( result )
    where result = isPrime val -- target that got tested


-- prop2. if a number n is returned by isPrime to be true (i.e prime)
    -- then it should have zero divisors.
prop_primesArePrimes val = if isPrime val == Just True
                           then length (divisors ) == 0 
                           else True
    where divisors = filter ( (== 0 ) . (mod val)) [2 .. (val - 1)]                        


-- prop3. check that if your function returns Just False, 
--     the input number has at least one number that’s less than it 
--          and evenly divides it.
prop_nonPrimesAreComposite val = if target_result == Just False
                                 then length (divisors) > 0
                                 else True
    where divisors = filter ( (== 0 ) . (mod val)) [2 .. (val - 1)]
          target_result = isPrime val                                
-- this one caught an error, isPrime 0 was returning Just False,
-- and not generating any divisors heres.
-- this is incorrect because isPrime 0 == Just False, means it's composite,
-- when in fact that is not true
-- so, we refactored isPrime to return Nothing if number was < 2