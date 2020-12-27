import Data.List
-- composability:

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

instance Semigroup Integer where          
   (<>) x y = x + y    


instance Semigroup Double where          
   (<>) x y = x * y   

-- semigroups should be associative
-- i.e a <> ( b <> c ) == ( a <> b ) <> c
-- Haskell does not enforce this, 
-- but the programmer should design their classes in such a way that associativity is maintained

{-
*Main> :t (<>)
(<>) :: Semigroup a => a -> a -> a

-}

