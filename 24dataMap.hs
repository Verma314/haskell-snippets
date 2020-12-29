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




{-
Q18.2

Modify the Organ type so that it can be used as a key. 
Then build a Map, organ-Inventory, of each organ to its count in the organCatalog.
-}

data OrganV2 = HeartX | BrainX | KidneyX | SpleenX deriving (Show, Eq, Ord)

myOrgans :: [OrganV2]
myOrgans = [HeartX,HeartX, HeartX, BrainX, KidneyX, SpleenX, SpleenX]

-- construct a OrganV2 list and Count list

getCount :: [OrganV2] -> OrganV2 -> Int
getCount myOrgans organ = foldl (+) 0 flipCorrectOrganOn 
                          where flipCorrectOrganOn =  map (checkOrganExistence organ) myOrgans
                                checkOrganExistence organ x = if x == organ then 1
                                                        else 0 

getUniqueOrgans :: [OrganV2] -> [OrganV2]
getUniqueOrgans [] = []
getUniqueOrgans (firstOrgan:givenOrgans) = if  elem firstOrgan givenOrgans then getUniqueOrgans(givenOrgans)
                                           else firstOrgan : getUniqueOrgans(givenOrgans)



getUniqueOrganCountPairs :: [OrganV2] -> [(OrganV2,Int)]
getUniqueOrganCountPairs givenList = map (getUniqueOrganCount ) uniqueList
                                where  uniqueList = getUniqueOrgans(givenList) 
                                       getUniqueOrganCount organ = (organ,countOfOrgan)
                                                                  where countOfOrgan = getCount myOrgans organ
                                                                   
                                                                    
myOrgansUniqueCount = getUniqueOrganCountPairs myOrgans 

myOrgansUniqueCountMap = Map.fromList myOrgansUniqueCount              
getCountOfHeartX = Map.lookup HeartX myOrgansUniqueCountMap

{-
*Main> getCountOfHeartX 
Just 3
-}

-- Works!