
myMap _ [] = []
myMap function list = function (head list) : myMap function (tail list)


--define myMap2 ( function, array )  = return (  function( head array ) :    myMap2 ( function, array[1::])  

myFiler _ [] = []
myFiler criteria list = if criteria (head list) then (head list ) : myFiler criteria (tail list)
                        else myFiler criteria (tail list)


myFiler2 _ [] = []
myFiler2 criteria list = if criteria (head list ) then (head list) : restOfTheCriteriadList
                         else restOfTheCriteriadList
                         where restOfTheCriteriadList =  myFiler2 criteria (tail list)
                         



-- implement remove:
myremove _ [] = []
myremove criteria list = if criteria (head list) then restOfTheCriteriadList
                       else (head list) : restOfTheCriteriadList
                       where restOfTheCriteriadList = myremove criteria (tail list)



-- implement reduce/foldl
reduce _ inital [] = inital
reduce operator inital list = reduce operator (operator inital (head list)) (tail list)

-- implement foldr 
reduce2 operator initial [] = initial
reduce2 operator inital list = operator (head list) ( reduce2 operator inital (tail list))


{--
reduce +  0     [1,2,3,4,5]
reduce +  1     [2,3,4,5]
reduce +  3     [3,4,5]
.
.
reduce + 15     []

====================

(reduce + 0 [1,2,3,4,5])
1   +   (reduce + 0 [2,3,4,5])
1   +   2        +  (reduce + 0 [3,4,5])
1    +  2  +        3     + (reduce + 0 [4,5])
1   +   2  +        3     + 4 + ( reduce  + 0 [5] ) 
1   +   2  +        3     + 4  + 5 + (reduce + 0 [] )    
1   +   2 +         3     + 4  + 5 + 0



--}


-- implementing reduce_left and reduce_right again for practice:

-- tail recursion (no stack created)
reduce_left op init [] = init
reduce_left op init (x:xs) = reduce_left op (op init x) xs

-- right_reduce, stack created
reduce_right op init [] = init
reduce_right op init (x:xs) = op x (reduce_right op init xs)




-- concat all strings of a list
-- [ "Sdadsa", "sadasd", "Sadasdas", "adsda"]

concatAll [] = []
concatAll list = foldl (++) "" list


--  create sumOfSquares, which squares every value in a list and then takes the sum of it:
sumOfSquares list = foldl (+) 0 (map (^2) list)


-- use foldl to reverse a list
rcons x y = y:x
myreverse2 list = foldl rcons [] list



-- create function elem
myElem element list = if length filteredList >= 1 then True
                      else False
                      where filteredList = filter (== element) list


-- alternate:
myElem2 element list = foldl (||) False list3
                       where list3 = (map (== element) list)



{-- In mathematics, 
the harmonic series is the sum of 1/1 + 1/2 + 1/3 + 1/4 ....... + 1 / n
 Write a function harmonic that takes an argument n 
 and calculates the sum of the series to n. Make sure to use lazy evaluation. --}



harmonicSeries n = foldr (+) 0 harmonicList
                    where harmonicList = map (\x -> (1/x)) [1..n]

-- ^ todo lazily
-- https://stackoverflow.com/questions/61473630/compute-harmonic-function-lazily
                    





