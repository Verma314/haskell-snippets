x :: Int -- tyoe signature
x = 2

-- without the type signature Haskell would figure out the type of x by infrence during compile time

x1 = x ^ 1000 -- Int is a bounded type


y :: Integer
y = 2 ^ 2000


-- other common types
char :: Char 
char = 'c'

rate :: Double
rate = 0.1234342385235782

fun :: Bool
fun = True


-- List types
charList :: [Char]
charList = ['a','b','c']

doubleList :: [Double]
doubleList = [1.21,4.52,6.532]

values :: [Int]
values = [1,2,3,4,5]

anotherCharList :: String
anotherCharList = "my char list"


nameAgeHeight :: (String,Int,Int)
nameAgeHeight = ("Random Person", 34,180)

------------------------------------------------------------
----- type signature of a function -------

double :: Int -> Int
double x = x * x


half :: Int -> Double
half x = (fromIntegral x) / 2

----- 
-- to convert from string to read
aNumber :: Int
aNumber = read "6"


-- to conert number to string
aString :: String
aString = show 100

--------------------------------------------------------
----- Functions with multiple arguments ----------------
multipleArgsExample :: Int -> String -> String -> (Int, String, String)
multipleArgsExample number street town = (number, street, town)


--------------------------------------------------------
----- Functions which accept 1st class functions -------

ifEven :: (Int -> Int) -> Int -> Int
ifEven func x = if even x
                then func x
                else x

test0 = ifEven (\ x -> x  + 5) 10

--------------------------------------------------------
-------------------- type variables --------------------
simpleInt :: a -> a
simpleInt n = n


makeTriple :: Int -> b -> Int -> (Int,b,Int)
makeTriple rank1 name rank2 = (  rank3, name, rank4)
                              where rank3 = (rank1 + 1)
                                    rank4 = (rank2 + 1)

--------------------------------------------------------
--------------------------------------------------------

myTail :: [a] -> [a]
myTail [] = []
myTail list = (tail list)


