import Data.Char
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

-- to do:  guard function


-- new stntax for list comprehension:
-- squares
squares = [ x ^ 2 | x <- [1..10] ]

squares2 = [ y ^ 2 | x <- [1..10], let y = x + 11 ]

--powersOf2and3Alt:
powersOf2and3Alt :: Int -> [(Int,Int)]
powersOf2and3Alt n = [ (powerOf2,powerOf3) | element <- [1..n] , let powerOf2 = element ^ 2,
                                                                    let powerOf3 = element ^ 3  ]



--- exercises:
-- Write a list comprehension that takes the following words

-- ["brown","blue","pink","orange"]
-- and capitalizes the first letter, and prepends Mr. in front.                                                                    

myElements = ["brown","blue","pink","orange"]

stringOps names = [ "Mr " ++ newName | element <- names, 
                            let newName =  (Data.Char.toUpper (head element)) : (tail element)]


{-
todo:

write a post,
how are list comprehensions a type of monad function?

how are list comprehension implemented?

they are a syntactic sugar for do notation.
which is a syntactic sugar in itself for monad's bind (>>=) function,
which helps us in chaining.

The type signature for bind is:

bind:: functionWhichTakesinANormalElementButReturnsAnElementInAContext -> anElementInContext -> anElementInContext

or,

bind :: (Monad m) => (a -> m b) -> m a -> m b 

-}


{-Use a list comprehension 
that generates a list of correct calendar dates, 
given that you know the number of days in each month. 
For example, 
it should start with 1 .. 31 for January 
and be followed by 1 .. 28 for February.-}


monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

generateDates monthEnds = [ days | end <-  monthEnds, let days = [1..end] ]

-- Translate the preceding question into do-notation, and then into Monad methods and lambdas.
generateDatesDo monthEnds = do
                            end <- monthEnds
                            let days = [1..end]
                            return days

-- monad methods                            
generateDatesMonads monthEnds = monthEnds >>= (\ x -> return [1..x])