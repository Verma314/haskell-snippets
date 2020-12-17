----- create a cup "object"

-- cup fl0z = (\ _ -> fl0z)


-- constructor:
cup fl0z = \ message -> message fl0z


-- create objects:
aCup = cup 6
coffeeCup = cup 12

-- create messages:
-- NOTE that in Haskell, messages and objects interact liket this <message> <object>
-- get the number of Oz from an object

getOz aCup = aCup (\x -> x)


-- drinkFromCup oz aCup = cup (aCup ( \ x ->  x - oz ) )

drinkFromCup aCup ozDrank = if (fl0z - ozDrank >= 0) then cup ( fl0z - ozDrank)
                            else cup 0
                            where fl0z = getOz aCup



-- another message
isCupEmpty aCup = getOz aCup == 0


afterOneSip = drinkFromCup coffeeCup 1
afterTwoSips = drinkFromCup afterOneSip 1
afterGulp = drinkFromCup afterTwoSips 4

-- multiple sips:
leftoverCup = foldl drinkFromCup coffeeCup [ 1,1,1]

multipleSips myCup sips = foldl drinkFromCup myCup sips

-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------

-- Robots