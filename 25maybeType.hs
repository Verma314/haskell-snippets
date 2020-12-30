import qualified Data.Map as Map

groceries :: Map.Map String Int ; 
groceries = Map.fromList [("Milk",1),("Candy bars",10), ("Cheese blocks",2)]

getAMaybe = Map.lookup "Randomdaskda" groceries
getMilk= Map.lookup "Milk" groceries

{-
when we lookup we are returned a Maybe Int both times

*Main> getAMaybe
Nothing
*Main> :t getAMaybe
getAMaybe :: Maybe Int
*Main> 


*Main> 
*Main> getMilk 
Just 1
*Main> 
*Main> :t getMilk 
getMilk :: Maybe Int

-}

-- from previous file
-- begin:
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
-- end;


-- let us say we have forgotten which drawer contains the organs 
-- and want to query all drawers from 1 to 50.
-- how do we look that up?

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- let us retrieve the contents of all drawers
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids organCatalog = map lookUpId ids
                                     where lookUpId id =  Map.lookup id organCatalog

-- note the sheer number of times we tried to look up something that did not exist
-- is there a clean way to do this in OOP/Imperitive languages?
-- without having to deal with tonnes of nulls or extensive exception handling?

drawerContents :: [Maybe Organ]                                    
drawerContents = getDrawerContents possibleDrawers organCatalog                                    



-- to count the number of times an organ occurs in our list:

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\x -> x == Just organ)
                                      available)

countHeart = countOrgan Heart drawerContents                                       


isAnOrgan :: Maybe Organ -> Bool
isAnOrgan (Just _) = True
isAnOrgan Nothing = False

filterOrgans :: [Maybe Organ] -> [Maybe Organ]
filterOrgans organs = filter isAnOrgan organs

{-
test:

*Main> filterOrgans drawerContents 
[Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]


Works!
-}


-- Write a function that takes a Maybe Int, returns Int if Maybe Int, else returns 0

unwrapMaybe :: Maybe Int -> Int
unwrapMaybe (Just n) = n 
unwrapMaybe Nothing = 0    