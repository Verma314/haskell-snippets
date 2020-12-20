-- let us say we want to write an increment fxn which works on a variety of numerical types
-- it is entirely possible if we do not specify the type signature.
-- But can we write a type signature for a fxn "increment" which works on all numbers.


{-
 *Main> :t (+)
 (+) :: Num a => a -> a -> a

When we ask Haskell for the type of (+), 
there are a few additional symbols in there:
Num a => 

This is a type class.

It means that 'a' is a type of of class Num.

What does that mean?

"Num" is a type class generalizing the idea of a number.
And all types of a class Num must have a function (+) defined on them.


We get more information from:

:info Num

which lists out all the functions that all members of the class Num.
All types that are a Num must must implement these functions
and the type signature of all these methods.

/ is not there because / is not defined on all cases of Num

-}


{-

*** Why do we need type classes at all? ***

If we dont have type classes, for example:
for each type we would need a different add function 
like one with Double -> Double ->Double, Int -> Int -> Int



Infact,

We can use type classes to define functions which work on a variety of types.

-}


-- here, a is a type of class Num
-- and, this fxn accepts two Nums of type "a", and returns a type "a" 
addThenMultiply :: Num a => a -> a -> a
addThenMultiply num1 num2 = ( num1 + num2 ) * 2

-- GHCI examples,
-- addThenMultiply 3 2.000
-- addThenMultiply 3 2



----------------------------------------------------------------------------
---------------------- Defining a type class -------------------------------

-- output of :info Num is the definition of the type class Num

{-
class MyNum a where
    (+) :: a -> a -> a 
    (-) :: a -> a -> a
    (*) :: a -> a -> a
-}

-- definiting our own type class Describable

class Describable a where
    describe :: a -> String

-- ?? what do I do with Describable now?

----------------------------------------------------------------------------
---------------------- Common type classes ---------------------------------

-- Ord is a type class  which:
-- "represents all of the things in the universe that can be compared and ordered. "


-- if we use :info <type>, example: :info Int, we get all the type classes for which Int is a member of

-- Show and Read are also type classes which implement the functions show and read.    
-- any time a value is printed to the ghci it is because it is a  memeber of the type Show

-- for types which have not implemented show, will not be able to print to the console
-- Example:

data Icecream = Chocolate | Vanilla

-- Icecream is nearly identical to Bool 
-- but Bool implements show, and Icecream does not


-- this is not working :
{- 
showme :: Show Icecream => Icecream -> String
showme Chocolate = "Chocolate"
showme Vanilla = "Vanilla"
-}

instance Show Icecream where
    show Chocolate = "Chocolate"
    show Vanilla = "Vanilla"
-- boom    

-- earlier it used to say:
{-
<interactive>:451:1: error:
    • No instance for (Show Icecream) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
-}
-- so I created an instance 

------------------------ Deriving type classes --------------------
--  we can certainly derive from a type class too
-- we can derive Show into our custom type like this:


data ChildTypeRandom = RandomOne | RandomTwo deriving (Show, Eq, Ord)
data MyRandomType = MyRandomType ChildTypeRandom  deriving (Show)
-- Haskell automatically implemented the type class Show for us here



z = MyRandomType RandomOne
--- MyRandomType RandomOne

