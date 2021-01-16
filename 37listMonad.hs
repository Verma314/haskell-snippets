-- revision
readInt :: IO Int
readInt = read <$> getLine

readInt2 :: IO Int
readInt2 = do   
           element <- getLine
           return (read element)

-- revision end;

-- What’s the simplest way to create a list of the square of every odd number less than 20?
squareAllOdds :: [Int]
squareAllOdds = do
                let originalList = [1..20]
                let modified = filter (\ x -> ((mod x 2) == 1)) originalList
                element <- modified
                let modifiedElement = element ^ 2
                return modifiedElement


squareAllOdds2 :: [Int]                
squareAllOdds2 = do
                let modified = filter (\ x -> ((mod x 2) == 1)) [1,3..20]
                element <- modified
                let modifiedElement = element ^ 2
                return modifiedElement                

-- we can directly take elements out of the list
squareAllNums :: Int -> [Int]
squareAllNums n = do
                element <- [1..n]
                return (element ^ 2)        

powersOfBoth2and3 :: Int -> [(Int,Int)]
powersOfBoth2and3 n = do  
                    element <- [1..n]
                    let powerOf2 = element ^ 2
                    let powerOf3 = element ^ 3
                    return (powerOf2,powerOf3)                       

-- to do:  guard functions