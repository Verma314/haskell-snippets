--experiments

x = 1
-- not allowed x = 2

mysum x = x + 1

mysum2 y = do
    x + 1
 
--reward is that you always know that after calling a function, the world remains the same.

-- functions can not change the state of the program, cant change variable values etc   

---------

-- if then else
calcChange given owed = if (change > 0) 
                        then change
                        else 0
                        where change = (given - owed)
                        
