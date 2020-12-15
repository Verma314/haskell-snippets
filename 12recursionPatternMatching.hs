-- Implement 'take
-- Example:
-- take 3 [1,2,3,4]
-- [1,2,3]

myTake num (elem:lis) = if num > (length (lis) + 1) then (elem:lis) -- edge case:
                        else if num == 0 then []
                        else elem : (myTake (num - 1) lis) 
                
-------------------------------------
-- Implement GCD of two numbers
-- GCD == largest number that divides both the numbers

-- to do
------------------------------------

-- switch case:

sayAmount n = case n of 
              1 -> "one"
              2 -> "two"
              n -> "lots"


-- same logic above using pattern matching:

sayAmount2 1 = "one"
sayAmount2 2 = "two"
sayAmount2 n = if  even n then "lots" else "no"

-- more pattern matching

isEmpty [] = error "Empty list"
isEmpty _ = False

-- even more:

myHead (x:xs) = x
myHead [] = error "No head for empty list"

-- my tail

myTail (_:xs) = xs



-- The tail function in Haskell returns an error when called on an empty list. Modify myTail so that it does handle the case of an empty list by returning the empty list.

myTail [] = []


