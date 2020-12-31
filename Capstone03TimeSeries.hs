import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe


file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]


data TS a = TS [Int] [Maybe a] deriving (Show)

createTS :: [Int] -> [a] -> TS a
createTS times values = TS sortedTimes correspondingValues
                        where sortedTimes = [minimum times..maximum times]
                              tsMap = Map.fromList (zip times values)
                              correspondingValues = map ( \ time -> Map.lookup time tsMap) sortedTimes


--files defined above are not in the right format, 
--so we define methods to unzip the files and create a time series out of them.s
fileToTS :: [(Int,a)] -> TS a
fileToTS file = createTS timeSeries values
                where timeSeries = fst seriesValues
                      values = snd seriesValues
                      seriesValues = unzip file


-- todo implement a proper Show for our TS type

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4


-- we will write a method that can insert a maybe to our map
insertMaybePair :: Ord k => Map.Map k a -> (k,Maybe a) -> Map.Map k a
insertMaybePair myMap (key,Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap



-- we will write a method that can insert a pair to our map
insertPair :: Ord k => Map.Map k a -> (k, a) -> Map.Map k a
insertPair myMap (key, value) = Map.insert key value myMap

-- data TS a = TS [Int] [Maybe a] deriving (Show)

-- we implemented the above so that we can combine two TS types

combineTS :: TS a -> TS a -> TS a                                        
combineTS ts1 (TS [] []) = ts1
combineTS (TS [] []) ts2 = ts2
combineTS (TS times1 maybeValues1) (TS times2 maybeValues2) = TS combinedTimes correspondingValues
                                                              where combinedTimes = removeDuplicates(times1 ++ times2)
                                                                    mapInitial = foldl insertMaybePair Map.empty (zip times1 maybeValues1)
                                                                    updatedMap = foldl insertMaybePair mapInitial (zip times2 maybeValues2)
                                                                    correspondingValues = map (\ time -> Map.lookup time updatedMap) combinedTimes 


removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates (xs)
                          else x : removeDuplicates(xs)



-- let us convert TS a into a Semigroup
instance Semigroup (TS a) where
    (<>) = combineTS

-- making TS an instance of a Monoid
instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)


tsAll :: TS Double
tsAll = mconcat [ts1,ts2,ts3,ts4]    


-- performing operations on the TS a

-- let us compute the mean of a list
mean :: Real a => [a] -> Double
mean elements = sumOfElements / count
                where sumOfElements = ( realToFrac . sum ) elements
                      count = ( realToFrac . length) elements

-- computing the mean of a Time Series TS

meanTS :: Real a => TS a -> Maybe Double
meanTS (TS [] [] ) = Nothing
meanTS (TS time values) = if all (== Nothing) values then Nothing
                          else (Just avg)
                          where avg = mean cleanVals
                                cleanVals = map fromJust (filter isJust values)


meanAll = meanTS tsAll



-- find diffs of two Maybes
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just a) (Just b) = Just (a-b)

-- let us now find the diff betweeen two TSes 
diffTS :: Num a => TS a -> TS a
diffTS (TS times []) = TS times [] 
diffTS (TS times maybeValues) =  TS times (Nothing:diffedValues)
                                where shiftedList = tail maybeValues
                                      zippedList = zip shiftedList maybeValues
                                      diffedValues = map (\ (x,y) -> diffPair x y) zippedList

-- todo: moving average, other exercises from Capstone                                      