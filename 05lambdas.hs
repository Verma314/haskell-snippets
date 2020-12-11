-- check if num is even/odd


y = (\x -> x) getLine

evenOddChecker x = if even x then 
                    x - 2
                    else
                    (3 * x )  + 1 

double =  \x -> (2*x) 


-- sqSum or sumSq challenge

-- given:

sumSqOrSqSum x y =  if sumSq > sqSum then sumSq
                    else sqSum
        where sumSq = ( x ^ 2  ) + ( y ^ 2 )
              sqSum  = ( x + y) ^ 2


--soln:

sumSqOrSqSum2 x y = (\ sumSq sqSum -> if sumSq > sqSum  then sumSq
                                else sqSum) ( (x ^ 2) + (y ^2)) ((x + y) ^ 2)



-- rewriting this one: 
{-
doubleDouble x = dubs*2
 where dubs = x*2                            
-}

doubleDouble x = (\ x -> x * 2 ) (x * 2)

-- in a way, we are implmenting the 'where' clause in Haskell here above
-- where is a clause in which we can create more variables

-------

--let can even let us overwrite variables


overwrite x = let x = 4
                in let x = 5
                    in let x = 6
                        in
                        x


--overwrite using just lambdas:

overwrite2 x = (\ x -> (\ x -> ( \x -> x ) 4 ) 5 )  6


-- whatever x we might pass, it gets overridden inside the annonymous block
overwrite3 x = (\ x ->  x)  6

--variables can ony be overriden using lambdas, let and where 

x = 100
overwrite4 x = x + 10
            where x = 1




