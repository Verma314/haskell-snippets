

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




