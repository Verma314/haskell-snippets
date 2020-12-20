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