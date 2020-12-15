-- Implement 'take
-- Example:
-- take 3 [1,2,3,4]
-- [1,2,3]

myTake num (elem:lis) = if num > (length (lis) + 1) then (elem:lis) -- edge case:
                        else if num == 0 then []
                        else elem : (myTake (num - 1) lis) 
                