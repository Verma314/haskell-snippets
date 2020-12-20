
-- implementing Show for our custom Type

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"


-- why can we not just create a new method called show? like this:
{-
show :: SixSidedDie -> String
show S1 = "one"
show S2 = "two"
show S3 = "three"
show S4 = "four"
show S5 = "five"
show S6 = "six"
-}

-- Because now when we try to invoke show, it gives us an error:
-- "Ambiguous occurrence ‘show’"


-- but we do want show to have different behaviours i.e Polymorphism 
-- this is achieved via Type Classes



--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
----- Implementing other Type Classes ------------------------------------------------------

-- S1 == S1 throws an error

-- to be able to compare two values of type SixSidedDie
-- we have to make it a member of Eq type class as well
-- which consquently means implmenting the (==) fxn

instance Eq SixSidedDie where
    (==) v1 v2 = (==) (show v1) (show v2) 

-- Short and sweet way to implement ^
    

-- Haskell has a "Minimal complete definition" for all of these Type Classes, that absolutely must be implemented if we wanna implement the Type Class

-- let us implement Ord, so that we can order the sides of the die  

-- Step 1. Lets get the :info on Ord

{-
*Main> :info Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  -}

  -- Step2. Check the minimal complete definition. 
  -- https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Ord
  -- that is: we need to implement compare, which is a method that returns an Ordering type

  -- Step 3. Lets check Ordering type
  {-*Main> 
*Main> :info Ordering
data Ordering = LT | EQ | GT    -- Defined in ‘GHC.Types’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Monoid Ordering -- Defined in ‘GHC.Base’
instance Ord Ordering -- Defined in ‘GHC.Classes’-}
-- it basically returns LT EQ or GT

-- Step 4. Lets implement Ord for SixSidedDie


-- we can convert it to string and then compare 
-- or do it manually

toNumber ::  SixSidedDie -> Int
toNumber S1  = 1
toNumber S2  = 2
toNumber S3  = 3
toNumber S4  = 4
toNumber S5  = 5
toNumber S6  = 6

instance Ord SixSidedDie where
    compare S6 S6 = EQ 
    compare S6 _  = GT
    compare v1 v2 = if ( toNumber v1 == toNumber v2 ) then EQ
                    else if ( toNumber v1 > toNumber v2 ) then GT 
                    else LT

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

-- ALTERNATIVELY to the above, ie implmenting type classes for ourselves, we could rely on haskell to derive them 

-- like:

data People = A | B | C | E | D  deriving (Show, Eq, Ord)

-- Using deriving ( Show , Ord ) etc can help us avoid writing potentially buggy code 


------------ Enum :

-- to implement Enum we need to only implement two of its methods "toEnum and fromEnum."
-- these methods are used to convert values from and to an Int

-- can be done like this:
instance Enum SixSidedDie where
   toEnum 0 = S1
   toEnum 1 = S2
   toEnum 2 = S3
   toEnum 3 = S4
   toEnum 4 = S5
   toEnum 5 = S6
   toEnum _ = error "No such value"

   fromEnum S1 = 0
   fromEnum S2 = 1
   fromEnum S3 = 2
   fromEnum S4 = 3
   fromEnum S5 = 4
   fromEnum S6 = 5

test0 = [S1 .. S6]   
test1 = [S1 ..] -- THIS although would cause an error

-- having derived your type class would have been a goog idea too

data SixSidedDieV2 = S01 | S02 | S03 | S04 | S05 | S06 deriving (Show, Enum, Ord, Eq)


test2 = [S01 .. S06]  
test3 = S01 > S01