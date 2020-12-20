import Data.List

-- creating a type synonym for Name

type Name = (String, String)
names :: [Name]
names = [   ("Aditya","Verma"),
            ("Richard","Feynman"),
            ("John","Smith") ]

sortedNames = sort names
-- this sorts by the 1st name, what if we want to sort the list by 2nd name?




{-Note that Enum doesn’t require either Ord or Eq, 
even though it maps types to Int values (which implement both Ord and Eq). 
Ignoring the fact that you can easily use deriving for Eq and Ord,
use the derived implementation of Enum to make manually defining Eq and Ord much easier.-}    
-- to solve the above implement Eq and Ord using the fromEnum method (to convert our value into num)

{-
Q14.2

Define a five-sided die (FiveSidedDie type). 
Then define a type class named Die and at least one method that 
would be useful to have for a die. Also include superclasses you think make sense for a die. 
Finally, make your FiveSidedDie an instance of Die.
-}

data FiveSidedDie = S1 | S2 | S3 | S4 | S5  deriving (Enum, Eq, Show) 
-- this deriving *needs* to be there because later we make FiveSidedDie an instance of type class Die
-- the type class Die itself has Eq and Enum as superclass
-- hence FiveSidedDie also needs to be able to implement Eq and Enum

class (Eq a, Enum a) => Die a where
    printSide :: a -> String


instance Die FiveSidedDie where
    printSide S1 = "S1"
    printSide S2 = "S2"
    printSide S3 = "S3"
    printSide S4 = "S4"
    printSide S5 = "S5"
