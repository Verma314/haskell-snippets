

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

mysum y = y + x             

-- whatever x we pass to overwrite4 it gets overwritten by 1




{-

implement this function using lambdas:

counter x = let x = x + 1
            in
             let x = x + 1
             in
              x
-}

counter x =  (\ x ->   ( \x -> x ) x + 1 ) x + 1 



-- more fun with overwriting stuff:

{-

implement this function using lambdas:

counter x = let x = x + 2
            in
             let x = x + 3
             in
              let x = x + 4
                in 
                    x
-}

counter2 x =  (\ x ->  (  \x -> ( \x -> x ) x + 4 ) x + 3 ) x + 2 



-- doesn't work and stays stuck. why does it stay stuck?
counter3 x = let x = x + 1
            in
             let x = x + 1
             in
              let x = x + 1
                in 
                    x

