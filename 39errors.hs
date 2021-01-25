
-- implementing take naively, missing a pattern,
myTake0 :: Int -> [a] -> [a]
myTake0 0 _ = []
myTake0 num myList = (head myList) : myTake (num - 1 ) (tail myList)





myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake num myList = (head myList) : myTake (num - 1 ) (tail myList)



myTake2 :: Int -> [a] -> [a]
myTake2 num myList |  (num <= 0) = []
                   |  (length myList ==  0) = []
                   |   otherwise =  (head myList) : myTake (num - 1 ) (tail myList)


myTake3 :: (Eq a) => Int -> [a] -> [a]
myTake3 num myList |  (num == 0) = []
                   |  ( myList ==  []) = []
                   |   otherwise =  (head myList) : myTake (num - 1 ) (tail myList)




--

myHead :: [a] -> a
myHead [] = error "empty list"    -- BAD PRACTICE        
myHead (x:_) = x

-- BAD PRACTICE because it should have been caught by the compiler, but might causes issues in runtime.

{-
Note,

-   maximum, succ, sum, ```/``` are also partial functions in prelude that fail on certain inputs.  ( empty lists, maxBound , infinite lists, what does ```/``` fail on?)
-}