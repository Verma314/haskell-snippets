
myMap _ [] = []
myMap function list = function (head list) : myMap function (tail list)


--define myMap2 ( function, array )  = return (  function( head array ) :    myMap2 ( function, array[1::])  

myFiler _ [] = []
myFiler criteria list = if criteria (head list) then (head list ) : myFiler criteria (tail list)
                        else myFiler criteria (tail list)



myFiler2 criteria list = if criteria (head list ) then (head list) : restOfTheCriteriadList
                         else restOfTheCriteriadList
                         where restOfTheCriteriadList =  myFiler2 criteria (tail list)
                         


















