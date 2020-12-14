-- to do cons, you need to pass an element that has the same type as the type of elements that a list contains

mycons x xs = x : xs

conCharToList list = 'a' : list

-- wont work:
-- onCharToList list = "a" : list


-- this would work:
conCharToList2 = 'a' : "ditya"


-- wont work:
--  test0 = 'a' ++ "ditya"

--works 
test1 = "a" ++ "asafaf"


--------------------------------------------------------
-- to get value from list:
myList x = [1,2,3,4,5] !! x

-- or

myList2 x = (!!) [1,2..] x



retrieveFromString string = (!!) string

retrievedValue = retrieveFromString "my TEST stringƒƒƒ" 

test2 = retrievedValue 1
test3 = retrievedValue 3
test4 = retrievedValue 6


-- example:
isPallindrome word = word == reverse (word)

---------

-- check if element is in list:

test5 = elem 'p' "piry3u4233"

-----------
-- retrieve n values

test7 = take 5 [1,2..100]
test8 = take 1000 [1,2..10]


-----------
-- drop n values from the beginning

test9 = drop 5 [1,2,3,4,5,6,7,8,9]

---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------

-- difference b/w cycle and repeat
-- cycle creates a list, it takes in a list, and repeats the elements of the list over and over again
-- repeat creates a list, it takes in a list, and creates a new list in which the original list is repeated over and over again

-- implement repeat

myRepeat list = cycle [list]

---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
 
-- Exercise 6.2 Will Kurt: 
-- implement a fxn 'subseq' that takes three arguments: a start position, an end position, a list, and return elements between them

subseq start end list =  drop start (take end list) 


---------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
 
-- Exercise 6.2 Will Kurt: 
-- Write a function inFirstHalf that returns True if an element is in the first half of a list, and otherwise returns False.

getFirstHalfOfList list = take ( div (length list) 2 ) list
inFirstHalf element list = elem element (getFirstHalfOfList list)

---------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
 