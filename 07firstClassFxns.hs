-- 

ifEvenApplyFxn x fxn = if mod x 2 == 0 
                        then fxn x
                        else x



doublex x = 2 * x



main :: IO()
main = do
    print (ifEvenApplyFxn 10 doublex)
    print (ifEvenApplyFxn 9 doublex)

 
 --z = ifEvenApplyFxn 9 doublex




filerLessThan y (x:xs) = if x < y then x : filerLessThan y xs
                         else filerLessThan y xs
     

exp2 :: Ord a => a -> [a] -> [a] -- why do we *have* to write this line?
exp2 _ [] = []
exp2 y (x:xs)  =   if x < y then  exp2 y xs
                else  x : exp2 y xs


-----------

ifEvenApplyFxn2 x fxn = if even x then fxn x
                        else x

k = ifEvenApplyFxn2 9 (\ x -> x ^ 3 )

myFunc :: IO()
myFunc = do
    print "Input number"
    input1 <- getLine
    let x0 = (read input1 :: Int)
    print (ifEvenApplyFxn2 x0 (\ x -> x * 2 ))

----
