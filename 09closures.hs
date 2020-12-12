ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n * 2
                 else n


-- functions as arguments:
ifEvenApplyFunc n func = if even n
                         then func n
                         else n

isEvenInc2 n = ifEvenApplyFunc n (\x -> x + 1 )


-- What we want is a function that builds ifEvenX functions.
genIfEvenX myCustomFunc =  ( \x -> ifEvenApplyFunc x myCustomFunc ) 



--  a function genIfXEven that creates 'a closure with x' and returns a new function that allows the user to pass in a function to apply to x if x is even.
genIfXEven x = (\ givenFunc -> ifEvenApplyFunc x givenFunc)