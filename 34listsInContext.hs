-- lists and applicatives

-- putting a function in a context:
--test01 = pure (minOfThree) <*> (readInt) <*> (readInt) <*> (readInt)

-- lists not just represent a container
-- they also represent context: i.e x = [1,2,3] means x can take values 1 or 2 or 3

-- using functions in the list context (using applicative), generates all possible values

test02 =  [(+)] <*> [1,2,3] <*> [10,20]

-- or better written as,
test04 = pure (+) <*> [1,2,3,4,5] <*> [10,20]

-- to generate all permutations 
concatCars a b =  a : [b]


{-
test05 = pure (concatCars) <*> "abc" <*> (pure (concatCars) <*> "abc" <*> "abc")

How can i chain it to be infinitely long?
-}
