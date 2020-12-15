-- Write your own version of drop and consider how this function is both similar to and different from take.
-- drop 3 [1,2,3,4]
-- [4]

myDrop number list = if number == 0 then list
                     else if number > length (list) then []
                     else myDrop (number - 1) (tail list)


-- take
-- take 3 [1,2,3,4]
-- [1,2,3]

myTake number list = if number <= 0 then []
                     else if number > length list then list
                     else (head list) : myTake (number - 1 ) (tail list)

-- comparison

-- both have an end goal in which no more elements are required to be dropped, or no elements are needed to be accepte
-- difference: the operation is slightly different

-----

-- implement length, use pattern matching 

myLength [] = 0
myLength (x:xs) = 1 + myLength (xs)


-- implement take using pattern matching

retake 0 _ = []
retake _ [] = [] -- edge case for when empty list is reached before the number
retake num list = head list : retake (num - 1 ) (tail list) 


-- take using closure 
takeX = (\ list num -> take num list )  

--testing:
takeFromMyList = takeX [1,2,3,4,5]
test_takeFromMyList = takeFromMyList 2
--testing end.


-- take using where

retakeWhere _ [] = []
retakeWhere 0 _ = []
retakeWhere num list = (head list) : rest
                         where rest = retakeWhere (num - 1) (tail list)

-- let's use 'where' more


-- implement drop using pattern matching and where:
dropX 0 list = list
dropX _ [] = []
dropX num list = dropX (num - 1) rest
                 where rest = tail list
                       

------------------------------------------------------------------
-- implement cycle

myCycle (x:xs) = (x : xs)  ++ myCycle (x:xs)

-- or

myCycle2 list = list ++ myCycle2 (list)


