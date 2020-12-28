-- parameterized types

-- simple parameterized type:

-- a box that contains other types
data Box a = Box a deriving Show

-- we can create a "new type" like:
type IntBox =Box Int


-- examples
n = 10 :: Int
intBox = Box n

{-
*Main> intBox 
Box 10
-}

word = "BOX" :: String
boxBox = Box word
---


-- We can make functions wrap/ unwrap functions too:

wrap :: a -> Box a
wrap a = Box a 

unwrap :: Box a -> a 
unwrap (Box a) = a

-- Box type is proabably not very useful
------

data TripleV2 = TripleV2 String String String deriving Show
-- * see, these parameterized types *are* types which basically allow us to include arbitrary data types and nothing else
-- without paramterized types we would be stuck with types that include very specfic types
data Triple a = Triple a a a deriving Show


-- let us create more types using this parameterized types:
type Point3D = Triple Double 

origin :: Point3D
origin = Triple 0.0 0.0 0.0

diagonal :: Point3D
diagonal = Triple 1.0 1.0 1.0


-- more types:
type FullName = Triple String

aditya :: FullName
aditya = Triple "A" "S" "Verma"


-- to create accessors:
first :: Triple a -> a
first (Triple x _ _) = x
-- these generic accessors can take any sort of Triple <type> type and return the first element

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]


-- write a generic function to transform a Triple
transform ::  Triple a -> ( a -> a)  -> Triple a
transform (Triple x y z) func = Triple (func x) (func y) (func z)

test = transform origin (\ x -> x + 1)
test2 = transform aditya reverse

----------------------------------------------------------------

-- the built in definition of list is like this:
-- data [] a = [] | a : [a]  
-- it is a type represented using [] which includes a type a 

-- then it is either an empty list [], or 
-- it is a cons of an element of type a and another list

-- although this definition uses built in syntax which we can not emulate like [Type] and also :

-- to implement our own type of List
data MyList a = Empty | Cons a (MyList a) deriving Show

-- let us create new type from our original parameterized type
type IntList = MyList Int

testList :: IntList
testList = Cons 1 ( Cons 2 (Cons 3 Empty ))

-- this is not valid :
-- testList2 = Cons 1 ( Cons 2 (Cons "hi" Empty ))


-- we can implement map for our generically created MyList
myMap :: ( a -> b ) -> MyList a -> MyList b
myMap func (Cons ele rest) = Cons (func ele) (myMap func rest)
myMap func _ = Empty


myNewList = myMap (\x -> x * 10) testList
myNewTestList = myMap customFunc myNewList
customFunc x = x * 11

