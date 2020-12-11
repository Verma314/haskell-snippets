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