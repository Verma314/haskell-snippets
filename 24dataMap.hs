import qualified Data.Map as Map
-- we qualify the import above because Data.Map shares some functions with Prelude
-- now, every function we use from Data.Map must be prefaced with Map


-- Example from the book 'Will Kurt Get Programming in Haskell'
-- let us create a new data type, Organ:
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)


organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs = zip ids organs
organMap = Map.fromList organPairs


