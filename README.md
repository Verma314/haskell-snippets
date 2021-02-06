# haskell-snippets

Notes I made, for quick reference and memory, while reading and solving exercises from ```Will Kurt's Get Programming in Haskell```

## One liners 

* Let and Where are both ways of creating variables. Choosing to use let or where is a matter of style the vast majority of the time in Haskell. 
* Lambdas can also be used to 'implement' variables example:
```
myFunction x y = (\ x y -> if x > y then x + y else x) (x ^ 2) (y ^ 2)
```
* "We are passing in a function and returning a lambda function. The function func that we passed in is captured inside the lambda function. When we capture a value inside a lambda function, this is referred to as a closure (on the function func). (From Will Kurt's Get Programming in Haskell book)
```
ifEvenApplyFuncClosure func = (\ x -> if even x then func x else x)
```
* Partial functions are critical
```
> add4 a b c d = a + b + c + d  
> adderPlus3 = add4 3
> adderPlus3 1 2 3
9
```
* Partial functions are the reason why arguments should be ordered from most to least general.

*  We can write a function which can take in a function as an argument, and flips the order of the function's argument 
```
flipBinaryArgs myfunction = (\x y -> myfunction y x)
```

* A built in Haskell function also exists called ```flip``` to flip binary arguments. To flip negative binary operator:
```
> flipedNegativeSign = flip (-)
> test = flip (-) 1 10
9
```
* "Closures combine lambda functions and first-class functions to give you amazing power." Will Kurt.

An example of this power
```

ifEven func x = if even x
                then func x     
                else x


ifEvenInc = ifEven ( \x -> x + 1)

ifEvenDouble = ifEven (\x -> x * 2 )

```


### lists: 

* "h" is a list of single characters in haskell, 'h' is a single list
```
['h'] == "h"
```

* basically you can cons an element of type T, with a list that contains elements of type T

* cons is not the same as combining two lists which is done using the ```++``` operator.

* "Haskell uses a special form of evaluation called lazy evaluation. In lazy evaluation, no code is evaluated until it’s needed. In the case of longList, none of the values in the list were needed for computation."

* biggest disadvantage of lazy evaluation is that it’s much harder to *reason* about the code’s performance

* implementing Repeat, which takes a list and repeats the list indefinitely
```
myRepeat list = cycle [list]
```

* Recusion rules (from W. Kurt's book)
1. Identify the end goals
2. What happens if the end goals are reached
3. List alternate possibilities.
4. Ensure alternate possibilities move towards the end goal

* Pattern matching can help a lot with recursion.

* Implementing cycle -- beautiful
```
> myCycle list = list ++ myCycle (list)
> take 9 (myCycle [1,2..100])
[1,2,3,4,5,6,7,8,9]
```

* Use foldl (reduce) to reverse a list
```
rcons x y = y:x
myreverse2 list = foldl rcons [] list
```

* implement foldl (tail recursive)
```
reduce_left op init [] = init
reduce_left op init (x:xs) = reduce_left op (op init x) xs
```

* implement foldr (stack is created
```
reduce_right op init [] = init
reduce_right op init (x:xs) = op x (reduce_right op init xs)

```

### Random

* ```instance Eq Int -- Defined in ‘GHC.Classes’```
means that Int implements Eq in GHC.Classes


## OOP using FP
* OOP "All objects can be viewed as a collection of attributes that you send messages to"

* Objects are constructed using:
```
myObject (prop1,prop2,prop2) = \message -> message (prop1,prop2,prop3)
```
These functions basically let us create a function -- which returns a function that is "blank" i.e. waiting for a function to be passed to it. 

Basically it is an "object" which lets us pass a "message" to it.
Or, it traps the arguments -- and we can pass any function (to be applied to the arguments) later.
Or, "objects" here are just data members which have this extra functionality to accept a message (ie a function).

* In Haskell, new "objects" are created by modifying copies of old, existing ones.


### How to generate Primes (and an example of Guard notation)
```
isPrime :: Int -> Maybe Bool
isPrime n   | n < 0 = Nothing
            | n > (length primes) = Nothing
            | otherwise = Just ( n `elem` primes)

-- where:            
primes :: [Int]
primes = sieve [2..10000]


sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (firstElement : rest ) = firstElement : sieve ( filteredRest )
    where filteredRest = filter (not . (== 0 ) . (`mod` firstElement))  rest

``` 
           
## Types

* "Haskell uses type inference to automatically determine the types of all values at compile time based on the way they’re used."

* "Int" is a type which represents how machines store numbers. "Integer" is a more true representation of the mathematical integer.

* A list of type [Char] is a string (or a list of characters) of *arbitrary* length. A tuple of type (Char) is a tuple with exactly one element -- fixed length.

* To convert from an Integral type to a fractional type:
```
half :: Int -> Double
half x = (fromIntegral x) / 2
```
* To make sure you get returned an integral type:
```
5 `div` 2
```
* Use ```show``` and ```read``` to convert to and from a string.
* Although when doing a read you need to specify the return type
```
read "6" :: Int
```
* Type Variable: a lowercase letter in a type signature indicates that any type can be used in that place. 
```
simple :: a -> a
simple x = x
```

### Creating types

* We can create a type **synonyms** by using the ```type``` keyword. Example
```
type FirstName = String
type SecondName = String
-- or 
type PersonName = (FirstName, SecondName)
```
* Creating a **new** type can be used by using the ```data``` keyword. Example
```
data ABOType = A | B | AB | O


showABO :: ABOType -> String
showABO value = case value of 
                    AB -> "AB"
                    A -> "A"
                    B -> "B"
                    O -> "O"
-- other examples:

type Age = Int
type FirstName = String

data Patient = Patient FirstName Age Int

-- or:

data Name =  Name FirstName LastName
           | NameWithMiddle FirstName MiddleName LastName

data Sex = Male | Female
data PaitientV2 = PaitientV2 { name :: Name, 
                                sex :: Sex,
                                age :: Int}
```

* These data types created using the record syntax can generate automatic getters and setters for us
```
jackie :: PaitientV2
jackie = PaitientV2 { name = Name "Jackie" "Smith",
                      sex = Female,
                      age = 42,
                      weight = 60,
                      height = 170,
                      bloodType = BloodType AB Pos}

-- getters:
test0 = age jackie
test1 = showBloodType (bloodType jackie )
test2 = showName (name jackie )

--setter:
jackieUpdated = jackie { age = 44 }
test3 = age jackieUpdated
test4 = showBloodType ( bloodType jackieUpdated)
```

## Type Classes
* "Type classes allow you to group types based on shared behavior"
* A type class states which functions a type must support. "Type classes require us to think in increasingly more powerful
forms of abstraction and form the heart of Haskell programming"

* A type ```a``` can belong to a type class (say) ```Num```, which generalizes the idea of a "number". There can be various other types that belong to ```Num```.  And all of them must implement certain functions. This info about a type class can be obtained via:
```
:info Num 
```
The definition is a list of functions that all members of the class must implement.

* Example:
```
-- here, ```a``` is a type of class Num
-- and, this fxn accepts two Nums of type "a", and returns a type "a" 
addThenMultiply :: Num a => a -> a -> a
addThenMultiply num1 num2 = ( num1 + num2 ) * 2
```

This function would work on Int, Double etc. Or on any other type that  a programmer has created,
and has implemented the Num type class.

### Defining a type class

* Can define a new type like:
```
class Describable a where
    describe :: a -> String
```
Now all types that choose to be a Describable have to implement this describe function.

* If we use :info <type>, example: :info Int, we get all the type classes for which Int is a member of.

* Example of an existing type class Bounded:
```
class Bounded a where
  minBound :: a
  maxBound :: a
```
* Here minBound and maxBound are not functions, but just fixed values:
```
Prelude> minBound :: Int
-9223372036854775808
```

#### In summary:

* Any type can belong to a generalized Type Class. Like ```:info Int``` shows the type classes that Int belongs to.
* We can define **new** functions using the Type Classes instead of Types in our function type signature, instead of individual types, for more generalization.
* If a type belongs to a particular type class, it must implement the functions specified by the type class...
*  ... Unless it derives them from the type class. Example: Haskell automatically implemented the type class Show for MyRandomType type here:
```data MyRandomType = MyRandomType ChildTypeRandom  deriving (Show)```
* We can also implement our own type classes.

* To get into about any class (type class, or type) use ```:doc <type>``` example:
```
:doc Int
```

* Type constructor and data constructor
```
data BloodType = BloodType ABOType RhType 
-- type^constructor   ^ data constructor
```

### Important + Interesting: Using Type Classes 

* Generally when we create our own type. We can include it as a member of one of these Type Classes by implementing the functions
specified in the documentation. 
```
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- to make elements of type SixSidedDie able to print, we need to implment Show (IE implement the method requred by Show to implement)
instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"
```
* to make us able to equate objects of this type, we include it under the Eq type class, and implement the function(s) required
```
instance Eq SixSidedDie where
    (==) v1 v2 = (==) (show v1) (show v2) 
```

* To make it comparable, we need to implment Ord. We see that Ord requires a minimum implementation of ```compare```
```
*Main> :info Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
```

The ordering type is defined as:
```
*Main> :info Ordering
data Ordering = LT | EQ | GT   
```

Let's implement Ord for SixSidedDie

```
toNumber ::  SixSidedDie -> Int
toNumber S1  = 1
toNumber S2  = 2
toNumber S3  = 3
toNumber S4  = 4
toNumber S5  = 5
toNumber S6  = 6

instance Ord SixSidedDie where
    compare S6 S6 = EQ 
    compare S6 _  = GT
    compare v1 v2 = if ( toNumber v1 == toNumber v2 ) then EQ
                    else if ( toNumber v1 > toNumber v2 ) then GT 
                    else LT
```

* We can also make ```SixSidedDie``` a part of Enum (ie make SixSidedDie an instance of TypeClass Enum) and then use the enum functions fromEnum to manually order Eq and Ord
```
instance Enum SixSidedDie where
   toEnum 0 = S1
   toEnum 1 = S2
   toEnum 2 = S3
   toEnum 3 = S4
   toEnum 4 = S5
   toEnum 5 = S6
   toEnum _ = error "No such value"

   fromEnum S1 = 0
   fromEnum S2 = 1
   fromEnum S3 = 2
   fromEnum S4 = 3
   fromEnum S5 = 4
   fromEnum S6 = 5
```

The type of fromEnum is
```
> :t fromEnum 
fromEnum :: Enum a => a -> Int
```
that is, it accepts an Enum type and returns an Int. Now SixSidedDie *is* an Enum as well.
hence we can use:
```
> fromEnum S2
1
> toEnum 1 :: SixSidedDie
two
```
* You can run  ```:info SixSidedDie``` to check all the type classes that SixSidedDie implements

* Although deriving these Type Classes is often a better idea than implementing everything
```
data SixSidedDieV2 = S01 | S02 | S03 | S04 | S05 | S06 deriving (Show, Enum, Ord, Eq)
test3 = S01 > S01
test2 = [S01 .. S06]  
```

## Define your own Type Class

* To DEFINE your own type class, which has requirement for one method implementation

```
Die a where
    printSide :: a -> String
```


* To DEFINE your own type class, which has some super classes:
```
class (Eq a, Enum a) => Die a where
    printSide :: a -> String
```

Make sure that all instances of Die (ie who ever choses to be a type of Die ) then are also implementing Eq and Enum. (i.e are also Enum and Eq themselves.) 

All instances of Die, (all types that are a Die/all types that implement Die) should also implement ```printSide```


* You can add type signatures for partial applications too.
For example in Capstone02Types...hs, there is function
```
rottN :: (Enum a, Bounded a) => Int -> a -> a
rottN int char = .... etc etc...

-- partial application, note type signature. It is bascically like the one from the above function except the Int!
rot13x :: (Enum a,Bounded a) => a -> a
rot13x = rottN 26                    
```

## Notes on 'Programming in Types' 
##### (from Get Programming in Haskell by Will Kurt.)

* A type signature is a description of a transformation. Types in Haskell allow us to view programs as a series of transformations. 

* By thinking about type transformations we can design an overall program in a similar way to designing a function. 

* In Haskell, we shall thing in and use types extensively. i.e Types first and using functions to flesh out the details.


* Types that we have seen so far are algebraic data types. That is, types created by 'and'ing two data types, or 'or'ing them.

Example: a Name type is a String and another String.
A Bool type is either ```True``` data constructor or ```False``` data constructor.


and types == product types
or types == sum types

* Nearly all programming languages support product types. Example: Structs in C, classes in Java, etc.

* By making new types only by combining existing types leads to a 'top-down' design. "We can only expand to an idea by adding to it"
"This is the basis for designing software in terms of class hierarchies."

* Lets say we want an abstraction for all the items being sold at a book store.
That is, all items will be represented by a certain class ```StoreItem```.

How do we do this if the items available in the store? i.e make this class searchable.


For example: if the store sells vinyls and books. Both have very different properties and can not be abstracted out easily. 

"The big problem is that you want a single type that represents both vinyl records and books so you can make a searchable inventory. Because you can compose types only by and, you need to develop an abstraction that describes everything that records and books have in common. You’ll then implement only the differences in the separate classes. This is the fundamental idea behind inheritance"

Now we implement the code using a common class say ```StoreItem```. And implement product specific logic (Books and Vinyls) using conditionals.

What do we do if we want to later add a third type of item to our inventory?

What if the third type of item does not have attributes common with Book and Vinyl.

* Therefore, the design can get really complicated with using only ```product types``` even for simple cases.   



## Sum Types | Or Types

* They let you combine two types with an  ```or```

* Examples from the book:
```
A die is either a 6-sided die or a 20-sided die or ....
A paper is authored by either a person (String) or a group of people ([String]).
```

* data Bool = False | True
is an example of an ```or``` type.

## Composability

* The Sum Types discussed above are kind of unique to Haskell (or FP?) and lets us use design patterns not available in traditional programming languages.

* Composability is another divergence from regular software design.

* Composability means that you create something new by combining two like things."
Like, concat two strings, two lists to get a new list, two documents.
Each of these methods of combining types has a unique operator or function.

Example
```
example = "What" ++ " Are " ++ " the " ++ " alternatives" 
```

* We can combine functions by using a period ```.```
Example:
```
myLast :: [a] -> a
myLast = head . reverse
```

### Combining like types: Semigroups 

* Any type can be made a type of the Semigroup type class.
Example
```
instance Semigroup Integer where          
   (<>) x y = x + y                       
```

### Monoids

* Monoids are semi groups with one more constraint: a type implmenting a monoid type class must also contain the identity element.

* Even though it seems like semigroup should be a superclass of monoid in Haskell. But no, Monoid is not implemented as a subclass of Semigroup.

* Definition of Monoid:
```
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

* the ```mempty``` is the identiy element. ```mappend``` is used in monoid as the operator instead of ```<>``` 

`
* the ```[]``` in Haskell, also implements monoid. So:

```
GHCi> [1,2,3] ++ []
[1,2,3]

GHCi> [1,2,3] <> []
[1,2,3]

GHCi> [1,2,3] `mappend` mempty
[1,2,3]
```

* To make a type an instance of the Monoid type class, we just need to implement
```mempty``` and ```mappend```. Haskell automatically generates ```mconcat``` for us.

Type signature for ```mconcat``` is:
```
mconcat :: Monoid a => [a] -> a
```


* Haskell is able to infer the definition of ```mconcat``` using:
```
mconcat = foldr mappend mempty
```
Why not foldl?

*  From the book
"Note that the reason mconcat uses foldr instead of foldl is due to the way that foldr can work with infinite lists, whereas foldl will force the evaluation."



## Parameterized Types (very cool)

* Types can take arguments -- but by using type variables in their definitions. "Their arguments are other types." 

* Quote from GPWH Book "Parameterized types allow us to define generic data structures that work with a wide range of existing data."

* Slightly similar to generics in C# or Java. *Parameterized types let us create "containers" that can hold other types.* Like,
```
List<String> 
```
or,
```
Pair<Integer,String>
```

*  We use these generic types to constraint the type of values that the container can take.

* Basic example
```
-- a box that contains other types
data Box a = Box a deriving Show
```

* Previously, to create a new type we would use:
```
data TripleV2 = TripleV2 String String String deriving Show
```
See, now these parameterized types *are* types, that basically allow us to include arbitrary data types (while creating new data types) and nothing else,
```
data Triple a = Triple a a a deriving Show
```
These are like generic types.

* Examples:

```
type Point3D = Triple Double 

origin :: Point3D
origin = Triple 0.0 0.0 0.0


-- more types:
type FullName = Triple String

aditya :: FullName
aditya = Triple "A" "S" "Verma"
```

* A function to transform a Triple a
```
transform ::  Triple a -> ( a -> a)  -> Triple a
transform (Triple x y z) func = Triple (func x) (func y) (func z)
```
This function is different from map functions for Lists, because map lets us change the type.
Transforming (as defined above) does not.

* To implement our own List container (very interesting + important)
```
data MyList a = Empty | Cons a (MyList a) deriving Show

-- let us create new implementation of List (which contains Integers) type from our original parameterized type

type IntList = MyList Int
 testList :: IntList
testList = Cons 1 ( Cons 2 (Cons 3 Empty ))
```

* To implement a map for your custom list ```MyList```
```
myMap :: ( a -> b ) -> MyList a -> MyList b
myMap func (Cons ele rest) = Cons (func ele) (myMap func rest)
myMap func _ = Empty
```

* A tuple in Haskell is a type which is multi-parameterized. You can define your own 2-tuple like this:
```
data MyTuple a b =  None | Tup a b deriving (Show)

type IntStringTups  = MyTuple Int String

rollName1 :: IntStringTups;
rollName1 = Tup 1 "Aditya"
rollName2 = Tup 2 "Afdsa"
```


## Kinds

* In Haskell, just like Data and Functions have their type. Types have their own type as well.
The type of a type is called a Kind.

* **Kind of Type** indicates the number of types that the type takes.  

* Types that take no parameters have a kind ```*```. Example,
```
*Main> :kind Int
Int :: *
```

* Types that take one parameter have a type ```* -> *```
```
*Main> :kind Triple
Triple :: * -> *
```

Kind of a two-tuple
```
*Main> :kind (,)
(,) :: * -> * -> *
```

* Kinds are of importance when we study monads and functors.



## Data.Map

* This is another parameterized type in Haskell.

* Module which has an implementation for Map
```
import qualified Data.Map as Map
```
* The function that helps us create a Map:
```
*Main> :t Map.fromList
Map.fromList :: Ord k => [(k, a)] -> Map.Map k a
```
We can see from the type signature that it takes a list of tuples,
the first element of the tuple must belong to an 'Orderable' type, (ie must inherit/implement Ord).

(This is because internally the Map is implmented via a binary tree. This is different from a hash map which is implemented using a hash function)

The ```fromList``` function then returns us a ```Map k a```

* So to create a Map use ```Map.fromList```
```
pairs = zip list1 list2
ourMap = Map.fromList pairs
```
The data ```ourMap``` will return us our Map. Example
```
*Main> ourMap 
fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]
*Main> 
```

* How do we lookup values from the map? Via ```Map.lookup```. An example:
```
*Main> Map.lookup 24 organMap 
Just Kidney
```
In general:
```
*Main> value = Map.lookup <key> <mapName> 
```

* When we print ```value```, we get
```
*Main> value
Just Kidney
```

Why the ```Just```? Let us check the type of ```value```
```
*Main> :t value 
value :: Maybe Organ
```
```Organ``` was the type of our value. 

* How to combine two Maps?

```
m1 = Map.fromList [(1,1),(2,2)]
m2 = Map.fromList [(3,3),(4,4)]

-- we will write a method that can insert a pair to our map
insertPair :: Ord k => Map.Map k a -> (k, a) -> Map.Map k a
insertPair myMap (key, value) = Map.insert key value myMap

-- how to combine:
m3 = foldl insertPair m1 (Map.toList m2)

-- how to convert insertPair into a lambda function?
```

* How to combine two lists into a single map?
```
mapInitial = foldl insertMaybePair Map.empty (zip keys1 values1)
updatedMap = foldl insertMaybePair mapInitial (zip keys2 values2) 
-- using fold to add many mant values
-- internally we are just doing Map.insert again and again

-- insert maybe pair takes in a map, and a (key,value) pair and creates a new map from the old map and with the updated values
-- method that can insert a maybe to our map
insertMaybePair :: Ord k => Map.Map k a -> (k,Maybe a) -> Map.Map k a
insertMaybePair myMap (key,Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap
```


## Maybe Type: another parameterized type

* Parameterized types (generic types, that let you create new types by "giving them" your chosen type as a parameter) are much more important in Haskell, than generics are in OOP languages.

* Unlike Lists or Maps which represent containers of values of a certain type.
A Maybe type represents a context for a value.

* Maybe types represent values that might be missing.

Instead of using null, "...the Maybe type allows you to write much safer code. Because of the power of the Maybe type, errors related to null values are systematically removed from Haskell programs."

* Say we quickly create a Map:
```
groceries :: Map.Map String Int ;  
groceries = Map.fromList [("Milk",1),("Candy bars",10), ("Cheese blocks",2)]
getAMaybe = Map.lookup "Randomdaskda" groceries
getMilk= Map.lookup "Milk" groceries
```

On checkin the types on GHCI,
when we lookup we are returned a Maybe Int both times:

```
*Main> getAMaybe
Nothing
*Main> :t getAMaybe
getAMaybe :: Maybe Int
*Main> 


*Main> 
*Main> getMilk 
Just 1
*Main> 
*Main> :t getMilk 
getMilk :: Maybe Int
```

* Maybe is a type in a context. The context being that the type might be missing. 
This parameterized type is not like the others discusses before, they all represent containers.

* Definition of Maybe
```
data Maybe a = Nothing | Just a
```
Any Maybe ```type``` is either nothing or it is just a value from that ```type```

### Why use Maybe Types?

* When looking up a value from a HashMap, the approach in a lot of programming languages is to return an **error** in case the value in not found.

This can go wrong easily because **where ever** (not just in Maps), a function possibly throws an error for such a scenario, the developer must catch the error and handle the exception, to make sure the program does not crash. These checks need to be placed everywhere. 

Freedom to handle it differently (or use alternate logic) is restricted. 

Example,
We might have wanted to handle the error differently depending on what value (that was expected) went missing. 

* Returning a ```null``` might lead to even more problems.  Null checks would have to be placed everywhere.
If a check is missed, they propagate further and cause more issues, that might not have been handled elewhere either. Null pointer exceptions are quite common.

* Why ```Maybe```? When a function returns a Maybe, the calling function can not avoid the fact that the data value is a ```Maybe```. "Maybe makes it impossible to forget that a value might be null."

* Maybe types are returned everywhere where there is a chance that the requested value might not exist.Example, opening files, RESTful API calls requesting a resource, reading from DB etc...

(Credit:examples from W. Kurt's Get Programming in Haskell)

* Let us say you have a list of Maybes, you can use isJust or isNothing to filter our the values from the Nothings
```
drawerContents  drop            dropWhile
*Main> filter Data.Maybe.isJust drawerContents 
[Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]
```


## I/O

* ```IO``` is also a parameterized type. Unlike a List (which is, again, a container), ```IO``` represents a context in which the value comes from an Input/Output operation.

Just like ```Maybe``` which represents a context in which a vaue might be missing.

* To verify the above:
```
GHCi> :kind Maybe
Maybe :: * -> *
GHCi> :kind IO
IO :: * -> *
```

* Examples, reading user input, reading from files, writing to console.s

* IO is inherently stateful, it changes the state of files.

* IO is also **impure**, refrential integrity is not followed. Because, a function can take in user input using ```getLine``` and return different values each time it is called.

A function might read a value from a file whose value changes (by other programs). Calling this function multiple times will return different values each time.

IO types are created to prevent pure and impure methods from mixing.

IO is also prone to errors. 

* Despite all the above, IO is important. 

From the book
```
What good is a program that doesn’t change the state of the world in some way? To keep Haskell code pure and predictable, you use the IO type to provide a context for data that may not behave the way all of the rest of your Haskell code does.
```



### More on I/O

* A method of a type  ```IO ()``` can **not** return anything.

Example, ```main :: IO()```


* Take the example
```
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
   putStrLn "Hello! What's your name?"
   name <- getLine                                      
   let statement = helloPerson name                     
   putStrLn statement     
```   


From the above:
1. ```getLine``` is an object of the type ```IO String```
2. ```name``` consequently is an ```IO String``` too
3. Whenever we need an IO String obejct to talk to the external world we use ```let```
4. ```putStrLn``` returns nothing. 
```putStrLn :: String -> IO ()```
5. ```main``` is not a function, it is an IO Action
"Some IO actions return no value, some take no input, and others don’t always return the same value given the same input."

*Other ideas directly from W. Kurt's book*

6. "The interesting thing about getLine is that you have a useful return value of the type IO String."
7. "Because I/O is so dangerous and unpredictable, after you have a value come from I/O, **Haskell doesn’t allow you to use that value outside of the context of the IO type***. For example, if you fetch a random number using randomRIO, you can’t use that value outside main or a similar IO action. " 
8. ". Because of this, after you’re working with data in the context of IO, it must stay there. This initially may seem like a burden. After you’re familiar with the way Haskell separates I/O logic from everything else, you’ll likely want to replicate this in other programming languages (though you won’t have a powerful type system to enforce it)."


### DO-notation

* Because one can not escape the IO context, one needs to find a way to perform sequence of computations within the IO context. The ```do``` keyword helps us with that.

* The ```do``` keyword allows us to use IO types *as if* they were regaular types.

* This is why both ```let``` and ```<-``` are used in the do block.

* **Variables assigned with ```<-``` allow us to act as though a type ```IO a``` is just of type a.**
Example
```
name <- getLine        
```
We can now pretend that ```name``` that is an ```IO String``` is a ```String```

* **We use let statements whenever you create variables that aren’t IO types.**
Example
```
let statement = helloPerson name  
```

Not using a let in a do block (when creating a non-IO) variable will give us a parse error.

Statement is just a normal String. We had to use it because we were getting the values from a function in a non-IO context.

* We had to use ```<-``` for ```name <- getLine```, because in the line after that we pass this IO String to a function that only accepts a String.

"Do-notation allows you to assign an IO String variable by using <-, to act like it’s an ordinary String, and then to pass it to functions that work with only regular Strings."

(How does this work?)

* If you need to accept a ```Double``` or another type from the console. You can use ```read```


* IO can use do-notation because it’s a member of a powerful type class called ```Monad```.
```Do``` is not specific to IO, and can be used by any member of ```Monad``` type class.

* ```Maybe``` is a part of the ```Monad``` type class too.

* "...the Monad type class allows you to write general programs that can work in a wide range of contexts."

* *Haskell handles all the dangers of IO by ensuring that all I/O logic is contained in an IO type.*

### Interacting with the command line

* To get arguments, we use ```getArgs``` function found in System.Environment. 

The type signature of getArgs is as follows:
```
getArgs :: IO [String]  
```

It is basically a list of strings but in the IO context. Hence the ```IO```

getArgs is important to take user input.

* To be able to map over this special list of strings, we need to use a different kind of a map function.
```
mapM_ putStrLn args
```
map (+ 1 ) [1,2,3]

mapM is a modified map to map ```Monad``` types.
mapM_  is modified mapM, such that it does not output a list. This is because the IO functions (called IO actions) do not return any values.

* Interacting with the command line, an Example
```
main :: IO ()
main = do
        args <- getArgs
        putStrLn "hi"
        mapM_ putStrLn args

-- to be able to use getArgs,
-- we need to compile it in the command line        
-- ghc 26ioIntro.hs
-- ./26ioIntro.hs 1 2 3
-- this outputs:
{-
hi
1
2
3
-}
```


* To get input from ghci itself, 3 times:
```
main :: IO()
main = do 
       userInput <- mapM (\ _ ->  getLine) [1,2,3]
       mapM_ putStrLn myMap
```

```<-``` is used because mapM returns the values inputted by the user, all of which are the IO String,
so it is returning ```[IO String]```

```<-``` will let us pretend that this is list is a normal string list.

* Instead of using the mapM and mapM_ technique above, 
Haskell has a function called ```replicateM```

```replicateM``` takes a number ```n``` and an IO action, and repeats the IO action ```n``` number of times.

Although, one needs to import ```Control.Monad``` before using this function.

```
> :t replicateM
replicateM :: Applicative m => Int -> m a -> m [a]
```


* Application of the above IO concepts:
```
-- we need to write a program that lets us sum n number of command line arguments,
-- this n is accepted dynamically from the prompt
-- then n values are read from the CLI
-- the sum of the n values is returned
main :: IO()
main = do   
       args <- getArgs
       let linesToRead = if length args > 0
                   then read (head args)
                   else 0
       numbers <- replicateM linesToRead getLine -- always remember, all the IO functions use <-, let is for normies
       let sumOfInputs = sum (map read numbers :: [Int])
       print (sumOfInputs) 
```       


* You can implement replicateM easily like this
```
myReplicateM n ioAction = mapM (\ _ -> ioAction) [1..n]
```

You can also run it directly on ghci as well,

```
> myReplicateM 4 getLine 
10
11
1
2
["10","11","1","2"]
```


###  Interacting with lazy I/O

* In our previous examples, the number of lines to be read is fixed. 
What if we have to keep on reading values from the console? 


* We have the IO type to separate IO functions from other Haskell functions. ```main``` should contain very little logic.

* In the example in the previous sub-section, we assumed that we *have* to deal with the inputs right away? Can we not defer it for the future?

* Instead of assuming that the inputs are discrete IO values inputted by the user,
let us treat user input like a *list of characters.*

* That will make it much easier to separate the IO from the logic. 

* For the above we use ```getContents```.
This lets us treat the stream from the terminal as a list of characters.

Using ```getContents``` we can completely re-write the program, assuming we have a list of characters -- and leaving the IO operations for later.

* Important: 
```getContents``` will keep asking for user input, until we send it an EOF,
which is ```Control-D```

* One can then pass the contents from ```getContents``` to regular Haskell functions
```
       contents <- getContents
       let numbers = toInts contents
       print (sum numbers)
```

This is the only time we have to do IO  / treat our list as IO.

Rest of the logic is written without the ```IO ()```


* Example of lAZY IO:
```
quoteGenerator  :: Int -> String
quoteGenerator 1 = "Hi QUote 1"
quoteGenerator 2 = "Hi QUote 2"
quoteGenerator 3 = "Hi QUote 3"
quoteGenerator _ = "Hi QUote Random"

main :: IO ()
main = do
       putStrLn "Enter a number"
       contents <- getContents
       mapM_ putStrLn (map quoteGenerator (toInts contents))
```
See, over here the promt can keep asking for input, 

because essentially contents is inside the mapM_
and we keep adding the the contents list.
Hence it keeps invoking the ```putStrLn```

*  In this example the prompt has to close
```
main4 :: IO ()
main4 = do
       contents <- getContents
       let numbers = toInts contents
       print (sum numbers)
```
It does keep asking the user for values basically, and keeps putting them into numbers. But once the Ctrl-D is pressed, there is no more addition to ```getContents``` and the result gets printed.

How do we make it reactive?
(not sure, attempt in 27lazyIO.hs)

## Text and Unicode

* String (a list of chars) is a really inefficient way to process Strings.
There is a another type called Text which will help us with string processing.


* Text is implemented as an array under the hood, unlike String which is implemented as a list

* Text does not use lazy evaluation.

* We have two important functions for String/Text conversion
```
T.pack :: String -> T.Text
T.unpack :: T.Text -> String
```

These conversion computations are not cheap.


* 
```
myWord :: T.Text
myWord = "random"
``` 
This above would throw an error, because "these literals" are Strings in Haskell.

And we **can not** fix this issue in haskell code.

You can pass a flag before compiling a hs program, or pass it as a flag when invoking ghci.

Example:

```$ ghc text.hs -XOverloadedStrings```

Another way, is to add a pragma on top of our file like,
``` 
{-# LANGUAGE <Extension Name> #-}
```

For text,
it will be:
```{-# LANGUAGE OverloadedStrings #-}```

This is a **language extension**

* almost all the  String function their corresponding version for working on Text in Data.Texts

Example. ```lines```


* See files, 28text.hs and 'Quick Text Operations.hs'

* Text is a monoid, and Text values can be combined using ```mconcat```

* ```putStrLn``` does not work for Text types.
However, ```TIO.putStrLn``` does work.

TIO needs to be imported
```
import qualified Data.Text.IO as TIO
```

```print``` might work, but it would not print unicode into the console, it would turn the symbols into its representation like ```\401``` etc


## Files

* To open a file, use this function:
```
openFile :: FilePath -> IOMode -> IO Handle
```

where FilePath is a string
```
type FilePath = String
```
And IO mode is,
```
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```


It returns an ```IO Handle``` which represents a reference to the file.

* To close a file ```hClose myFile```

* To read an write to file ```hPutStrLn``` and ```hGetLine```

Example,

```
firstLine <- hGetLine helloFile
```

* To check if the ```IO Handle``` has reached EOF:
```
hIsEOF file
```

Whose type signature is:

```
*Main T> :t hIsEOF
hIsEOF :: Handle -> IO Bool
```

### More IO tools

* We have some more IO functions which make reading and writing to a file easy.
To read:
```
input <- readFile fileName
```

to append:
```
appendFile "output.txt" "blah"
```

* These methods above do not close the handle. So one might run into strange lazy evalutation errors here (to research more on this).

To prevent that from happening, one can do is use Text instead of String.
Text is a strict data type, i.e it does not use lazy IO.

Example from W.Kurt Get Programming in Haskell:
```
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

getCounts :: T.Text -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
 where charCount = T.length input
       wordCount = (length . T.words) input
       lineCount = (length . T.lines) input

countsText :: (Int,Int,Int) -> T.Text
countsText (cc,wc,lc) = T.pack (unwords ["chars: "
                                   , show cc
                                   , " words: "
                                   , show wc
                                   , " lines: "
                                   ,  show lc])

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (countsText . getCounts) input
  TI.appendFile "stats.dat"
                (mconcat [(T.pack fileName), " ",summary, "\n"])
  TI.putStrLn summary
```

The author says,

"Strict evaluation means that your I/O code works just as you’d expect it to in any other programming language. Although lazy evaluation has many great benefits, for any nontrivial I/O, reasoning about its behavior can be tricky."

"As soon as your I/O becomes even moderately complex, involving reading and writing files, or operations for which order is important, stick with strict evaluation."


## Working with binary data

* ```ByteString``` allow us to treat binary data as if they were regular strings.

* ByteString is not for strings alone, but can deal with any sort of Binary streams.

* 

```
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

```Data.ByteString``` does not let us use ByteStrings as ```char```, so we use Data.```ByteString.Char8```, as below,


```
bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt givenByteString = (read . BC.unpack) givenByteString
```

* How to print unicode to the console?

```
import qualified Data.Text.Encoding as E
textWithUni = E.encodeUtf8 "Textƒƒƒ"
textWuthUnicode2 = E.decodeUtf8 textWithUni
TIO.putStrLn textWuthUnicode2 
```                    

* https://stackoverflow.com/questions/3951722/whats-the-difference-between-unicode-and-utf-8


## Functors 

*  "The Functor type class allows us to apply an ordinary function to values inside a container (for example, List) or a context (for example, IO or Maybe)"

* "Functor allow us to generalize by solving a single problem once, and automatically solves it for multiple parameterized types"

* "The Functor type class provides a generic interface for applying functions to values in a container or context."

* Consider these parameterized types (parameterized by Int)
```[Int]```, ```Map String Int```, ```Maybe Int```, ```IO Int```

These above are types that are *in a context.*

Let's say we have a function, ```X :: Int -> String```

Now without functors, we would need to write a customized version of ```X``` for all of the above parameterized types.
(i.e a separate version for a Maybe Int, IO Int, [Int] etc )

With functors, we will have a "uniform way" to apply a single function to all these parameterized types.



* Usefulness of tools such as ```Maybe``` are reduced if you have to keep implemeting for every function ```Func :: a -> b```, another function with a similar function, ```FuncMaybe :: Maybe a -> Maybe b```

A "special" version will need to be written for each of these functions, when implementing the logic with a type in a context.



* Now, Maybe is a member of the Functor type class
```
> :info Maybe
.
instance Functor Maybe -- Defined in ‘GHC.Base’
.
```
The above means that the Maybe type class implements the Functor type class.
(ie Maybe is a functor (? can we say that?))


* Let's look at Functor,
```
*Main> :info Functor 
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
.
.
```

This shows that the type class ```Functor``` is implemented by ```Maybe```,

and also that functor does't implement any other Type Classes (? to verify)

Another thing to note is:
For some type class to implement Functor they minimally need to implement ```fmap```.

```<$>``` is the same as ```fmap```


* Any type class that wants to be a Functor must implement:
```
fmap :: Functor f => (a -> b) -> f a -> f b
```

```fmap``` accepts a function (a->b)
and an argument ```f a```, 

here ```f a ``` is a functor of type a, example Maybe Int

* ```fmap``` **provides an adapter**, it is a binary operation


* Check ```32functors.hs``` for more explanation and examples.

* ```Maybe``` *is* a functor.
Which means we can pass a ```Maybe``` to fmap, and a regular function, and it will apply that for uss:

```
Prelude> fmap (+1) (Just 10)
Just 11

Prelude> fmap (+1) (Just 10134.3)
Just 10135.3

Prelude> fmap (++ "1") (Just "as")
Just "as1"

```

See how we no longer would need a function like this:
```
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing
```
Which takes in a Type (that is in a Maybe context) and applies a normal Int function to it -- wraps it and returns the maybe; we don't need to implement this.

We can just use ```fmap```,
which accepts the 'normal function definition', and a 'type in a context'


*  Works on any function that works on the nested type
convertLookedUpValToString =  fmap show (Just 10)
-- Just "10"

*  Reversing a Maybe String using functors:
```
reverseMaybeWithFunctors :: Maybe String -> Maybe String
reverseMaybeWithFunctors maybeStr = reverse <$> maybeStr
```
what is the functor here? is it the maybe? 

Again,
```
>:t fmap
fmal :: Functor f => (a -> b) -> f a -> f b 
```


```Functor f =>``` means that ```f``` being used in the type signature is a type of ```Functor```


seems like the ```Maybe``` is the ```functor```
Correct me if I am saying this incorrectly.


* A lot of these parameterized types have the kind 
``` * -> *```

Example, ```Maybe``` which takes in a type, and creates a new type for you.
```
type MI = Maybe Int

-- to use it:
x :: MI ; x = Just 10

```
another example,
```
*Main> :kind []
[] :: * -> *
```
All functors are of the kind ```* -> *``` .

*Many* parameterized types of the kind ```* -> *``` are instances of Functor.

* Imp: "Functors are incredibly useful because they allow you to reuse a single function with any type belonging to the Functor type class."

That means a Maybe *is* a functor.
* The fmap converts here a Maybe part type to a Maybe HTML

```
partHtml :: Maybe Html
partHtml = renderHtml <$> (Just part)
```

* a list of parts to a list of html

```
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
```

* a map of (int,part) to a map of (int, html)

```
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB
```

* **an IO of part to an IO of html**
```
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO
```

* ```<$>``` provides a common interface to apply any function to a value in a context. 

It helps with IO to change context -- but can not take things out of IO conext.

## Applicative

* Functors can be very useful when we have one value in a context.
What if two of our values are in a context. 

And we want to operate on them? And simultaneous produce the result which is in the same context.

Functors's fmap literally works on single argument function
```
fmap :: (a->b) -> f a -> f b 
```
(where f is any parameterized type which is a functor),

what if we want a map (?) which can work on multiple values?

like
```
requiredMap :: (a -> b -> c ) -> f a -> f b -> f c
```

### Partial functions with fmap

```map``` works like this: it takes in a unary function and a ```list```. And applies the operator on the values inside the list.

```fmap``` works similarly, it takes a unary function and a ```Functor```. And applies the operator on the values inside the Functor. 

(A functor is a List, or a Maybe, or an IO, etc)

Example: ``` map (+ 1) [1,2,3]```, returns:  ```[2,3,4]```.


**What if we do not provide a binary function to map, and pass it a binary function**
```
map (+) [1,2,3]
```

It returns a list of partially applied functions, ```[ (+) 1 , (+) 2, (+) 3]```

The type of this is ```[ a -> a ]```

Similarly, we can use ```fmap```
```
fmap (+) (Just 10)
```

which returns 
```
Just ( (+) 10 )
```

The type of which is ```Maybe (a -> a)```

Thus, a *function is a context* is created.



### **How to use the applicative**

Using the idea from [[03 Partial functions with FMAP]]

```
  maybeInc = (+) <$> (Just 10)
```

Note its type, ```maybeInc``` is a function in a context. (Parameterized function?)
```
> :t maybeInc 
  maybeInc :: Num a => Maybe (a -> a)
```


The **Applicative** type contains a function ```<*>```, whose type is
```
  (<*>) :: f (a -> b) -> f a -> f b
```

Applicative’s <*> allows us to use a function in a context. 

We can now use ```maybeInc``` :

```
> maybeInc <*> (Just 50)
Just 60`
```

To re-write:
```
> value = (+) <$> (Just 10)   <*> (Just 60)
> value
Just 70
```

**In Summary**

To create a generic function which re-uses functions, mapped from regular type and uses them with parameterized types:

```
applicativeFunc :: Applicative f => ( a -> b -> c ) -> f a -> f b -> f c 
applicativeFunc binaryfunc p_obj1 p_obj2 = usingApplicative
	where functionInAContext = fmap binaryfunc p_obj1
		  usingApplicative = functionInAContext <*> p_obj2
```

The ```applicativeFunc```  takes in a binary function, and two parameterized types, first we create a partial application by applying ```fmap``` with the binary operation and the first parameterized object. It creates for us an 
```f binary function p_obj1``` , which is a "unary" function.

(Remember that <\*> accept a unary function in a context, and another parameterized type.)

Now that we have our unary function in a context, and the second parameterized object, we return a ```functionInAContext <*> p_obj2```

Testing it out,

```
> applicativeFunc (+) (Just 10) (Just 30)	
Just 40
```


More examples,

```
> regularFunctionInParameterized (+) [1,2,3] [4,5,6]
[5,6,7,6,7,8,7,8,9]


> regularFunctionInParameterized (+) [1,2,3] [1]
[2,3,4]

```


### Operating on mulitple values in a context

We have read user input as int,
```
readInt :: IO Int
readInt = read <$> getLine
```

What if we have a function which takes not one (e.g. [[03 Partial functions with FMAP|fmap]]) argument. Not two ([[03-01 What if we want to pass two arguments to fmap|applicative]]) but an arbitrary number of arguments?

In this case, we apply the first argument to the function via fmap, thus creating a partial function in a context.  Then we apply the applicator's app ( ```<*>```) along with another argument (parameterized), which ends up creating another partial application, after which we apply another argument (parameterized), until we have applied all the arguments, and end up with a value in a context. 

Example,

take the function:

```
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)
```

Can we apply this function to values that are in an IO context? For example coming from ```readInt```.

Let us examine the partial functions (in a *context*) created on the way:
```
partialApplication1 :: IO (Int -> Int -> Int)
partialApplication1 = fmap minOfThree readInt
```

Now we can pass this ```partialApplication1```  to ```<*>```
```
partialApplication2 :: IO (Int -> Int)
partialApplication2 = partialApplication1 <*> readInt
```

Note how it takes a function in a context, and removes (?) it from context, removes the argument from the context, applies the function to the argument, puts it back in the context. (this is probably not how it works internally, although I might be wrong.)

And then we apply the ```<*>``` again,
```
result :: IO Int
result = partialApplication2 <*> readInt
```

Alternatively, we can write:
```
result2 = (fmap minOfThree readInt) <*> readInt <*> readInt
```


Based on partial applications, another cool thing we can do with ```<*>``` is
```
value = Just (+) <*> (Just 10) <*> (Just 20)
```

Where value becomes ```Just 30```!

Although Why does this not work: ```value = IO (+) <*> readInt <*> readInt ```
It throws the error: ```Data constructor not in scope: IO```

#question  How do we do the same with IO?

#question: How is <*> implemented internally?

#question: How do we put a function in an IO context?



### Applicative and Data in a context

Let us say we want to create a value of a certain type, (e.g. an _'and'_ type which contains some data), but all the data we have is in a context.

Example,
```
data User = User {name :: String, id :: Int, score :: Int} deriving (Show)
```

We know that the data constructor ```User``` works as a function as well. 

User's type, 
``` 
>  :t User
User :: String -> Int -> Int -> User
```
It accepts a String, an Int, another Int, and returns a User. Hence, we can use our partial application fmap and ```<*>``` magic we talked about before.

```
(fmap User (Just "Aditya")) <*> (Just 100) <*> (Just 100)
```

To make it general:
```
maybeUser maybeName maybeId maybeScore = 
								User <$> maybeName <*> maybeId <*> maybeScore
```

(How does it work? User fmapped with maybeName:, the function User is applied inside the maybeName, it returns a function in a context.  This function is a context is ```<*>```  with maybeId: the maybeId's value is applied to the function (which needs 2 arguments) and ends up returning another function in a context. In the end, we 'app' it with maybeScore: the maybeScore's value is applied to the function, which returns a User, it is wrapped in a ```Just``` and returned.)


To test,
```
>  maybeUser (Just "A") (Just 10) (Just 10)
Just (User {name = "A", id = 10, score = 10})
```

* Notice that the type signature of the operator <*> is almost the  the same type signature as fmap, except the function argument is also in a context for <*>

```
fmap  ::  (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

**Putting data in the desired context**

* The ```pure``` function:

```
*Main> pure [10] :: Maybe [Int]
Just [10]
*Main> pure 10 :: Maybe Int
Just 10
*Main> pure 10 :: IO Int
10

> pure "string" :: Maybe String
Just "string"
```
**Putting functions in the desired context**
```
> pure ( + 1 ) <*> (Just 10)
Just 11
```

Slightly more complex,
```
pure (+) <*> (Just 10) <*> (Just 20)
Just 30
```

**Function in an IO Context**
```
*Main> pure (minOfThree) <*> (readInt) <*> (readInt) <*> (readInt)
100
12
1
1
```

Exactly what we wanted! (note, minOfThree is just a normal Int -> Int -> Int)


*  This also works
```
hello :: IO String
hello = pure "Hello World"
```

### **Parameterized types - Containers vs Context**

Parameterized types that represent a **container** are ones that represent a data structure.

Parameterized types that convey meaning beyond just the structure are types in a **context**, just like IO Types. Discussed [[03-04 Applicative, IO, and chaining|here]].


Looking back, for types in context can have a function in them.
Example : ```Maybe (+)```,  ```IO func```,
but might not always make sense to have a function in a container (? not sure)


**Note that Maybe, IO are types of applicative. Which makes sense as functions can easily find themselves contained in these, via partial application of fmap.**


A ```Data.Map``` or ```(, )```  are not applicatives -- these are functors.

(Any type that is a member of Applicative, could be viewed as a type in a context.)


A list is both a context and a container -- it has a structure. 

Lists  represent context: i.e x = [1,2,3] means x can take values 1 or 2 or 3

Using functions in the list context (us`ing applicative), generates all possible values.

Example
```
test04 = pure (+) <*> [1,2,3,4,5] <*> [10,20]
```
will generate the sums of all possible combinations from the two lists above.
```
[11,21,12,22,13,23,14,24,15,25]
```

## Monads

* A monad is used when we wanna chain together functions. 

* It is a type class with the kind ```(m :: * -> *) ```

* For a type to be a monad, it must implement the function ```bind```

```
 (>>=) :: m a -> (a -> m b) -> m b
```

* Note how it takes two arguments. One, a parameterized type. Two, a function which takes in a "regular" type and returns a parameterized type. 

The function bind applies the function after "unwrapping" the parameterized type argument, and returns a parameterized type.

* Because it returns a parameterized type, you can chain it using another bind ```>>=```, by chaining it with another function  that accepts a normal type but returns a parameterized type.

* This is especially useful to chain ```Map.loopup```

* ```Map.lookup``` accepts a key (and a map) and returns a maybe
``` k -> Maybe a ```

* Example (from Stackoverflow: https://stackoverflow.com/questions/44965/what-is-a-monad)

Take an example of straightforward chaining

```
streetName = getStreetName (getAddress (getUser 17)) 
```

What if one of these methods returns a ```Nothing```?
We would have to write conditional checks over and over again.


Instead,

```
(((Just 117 >>= getUser) >>= getAddress ) >>=  getStreetName)
```

```getUser```  would return another ```Maybe```.
```getAddress``` while normally might take a string, but has been transformed via ```>>=``` to work on a ```Maybe``` 

Example from GHCI
```
mapp =  M.fromList [ (1,2),(2,3),(3,4),(4,5)]

getValFromMapp val = M.lookup val mapp

result = ((M.lookup 1 mapp) >>= getValFromMapp) >>= getValFromMapp  
-- returns Just 4
```

More resources on Monads for fun:

* http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

* https://wiki.haskell.org/What_a_Monad_is_not

* IO Inside / IO Simplified
https://wiki.haskell.org/IO_inside#I.2FO_in_Haskell.2C_simplified

* Understanding Monads:
https://web.archive.org/web/20120114225257/http://ertes.de/articles/monads.html

* Monads for OOP
https://ericlippert.com/2013/02/21/monads-part-one/#more-461


* Alternate way to look at Monad's ```>>=``` function: 

This ```>>=``` bind function,

it pipes,

on the left hand side is always some data in a context, 

to the right side of ```>>=``` is a function that takes the type (without context),
and operates on the value and returns it in context.


* Example, to write an IO action which prompts the user, gets an input, and prints it out:


```
nameStatementIO2 :: IO ()
nameStatementIO2 = askForName >> 
				getLine >>= (\ n -> return (nameStatement n)  >>= putStrLn
```

You see, ```>>``` is used as it ignores everything on the left.

 - ```getLine``` obviously returns an ```IO String```
- the result is piped to a lambda which calls ```nameStatement``` wrapped as an IO
-  the ```>>=``` operates the lambda on the string wrapped inside the ```IO String```. and returns the results as an IO String,
-  The result is ```bind``` (or piped) to a  ```putStrLn``` which acts on the ```IO String```'s string, and returns an ```IO String```


### Do notation

 ```Do``` notation is syntactic sugar for ```>>```, ```>>=``` and ```(\x -> return (func x))``


* Monads are a great tool for code reuse.

* In DO notaation

```<-``` takes an individual element out of the context.

after which you can do operations on the indivial element -- using let expressions,

these let expressions you can also return an element,
it will put all these elements back in the context and return the value

Example,
```
-- <- is actually takes the element out of the context basically
assessCandidateList2 :: [Candidate] -> [String]
assessCandidateList2 candidates = do
                                 candidate <- candidates
                                 let passed = viable candidate
                                 let statement = if passed then "passed" else "failed"
                                 return statement
```


* A truly generic function to access candidate, one which can take any monad
```
assessCandidateX :: (Monad m) => m Candidate -> m String 
assessCandidateX candidateInContext = do
                                      candidate <- candidateInContext
                                      let isPassed = if (viable candidate) then "passed"
                                                     else "fail"
                                      return isPassed
```
See file 36monadsDoNotation.hs for examples.


* todo: talk more about list comprehension. cover guard function.

*  how are list comprehensions a type of monad function?

how are list comprehension implemented?

they are a syntactic sugar for do notation.
which is a syntactic sugar in itself for monad's bind (>>=) function,
which helps us in chaining.

The type signature for bind is:

bind:: functionWhichTakesinANormalElementButReturnsAnElementInAContext -> anElementInContext -> anElementInContext

or,

bind :: (Monad m) => (a -> m b) -> m a -> m b 


List comprehensions are a syntactic sugar for [[05-02 Do notation|Do notation]], 
The do notation is a syntactic sugar in itself for monad's bind (>>=) function, (>>) function, and returns.

These tools help us in chaining.

**Why is list comprehension same as Monad's bind?**

Look at bind's type signature ``` Monad m => m a -> (a -> m b) -> m b```

it takes a value in a conext; and takes a function which accepts individual elements from inside the context, applies this function of the element(s) in the first argument (that are inside the context), and returns the values put back into the context.

List comprehension is the exact same thing, list is a monad, we pass it to the list comprehension, the function acts on the individual elements inside the list, and returns their transformed version inside the list itself.


## Organizing Haskell Code

#### in the folder "38modules:
* the main function gets it's own module Main -- the IO action is handled here
* Pallindrome logic get it's own module Palindrome.


General Ideas,

- we’d like to keep the main IO action in a separate file from the rest of the business logic. The main module should primarily be concerned with the execution of the program. Logic in a separate file, such as ```Logic.hs```
- Logic gets it's own module/file.
- When you don’t explicitly tell Haskell that you’re in a module, Haskell assumes that you’re the Main module
- we can make this explicit by using the following line at the top of your file          ```module Main where```
- What do we do if we create a function or value that conflicts with one of the functions already defined in Prelude? We specify the module when using the function with the conflicted name, example                                                                               ```doubleLength = Main.length * 2```
- We can also hide functions inside a module, just like private methods. These methods won't be visible to the outside world.
- Example, ```module Palindrome(isPalindrome) where``` means that Palindrome module exports only the ```isPallindrome``` function.

- You can also selectively import functions too. 
- Importing ```import Data.Char (toLower,isSpace,isPunctuation)```, means that we are only importing these three functions from the ```Data.Char``` module.

- Each Haskell program has a main function, sometimes implicitly created. 

- To import the logic module into your main use:
    ```import qualified LogicModule```
- And then just compile the main, which will include the other imported modules.
- This is for trivial builds. For more complex ones, we use "stack"


## Building using Stack

Stack is a powerful build tool. What it does:
1. Provides an isolated installation of GHC
2. Handles installation of packages and dependencies.
3. Automates building of the project.
4. Helps with organizing and running tests.


To create a new project, ```stack new project-name```

it creates a new project in a new directory for you,
- ```the project-name.cabal``` is an important file, it contains all the project configurations. i.e all metadata related to the project.

- in the cabal file, under ```library```, under ```hs-source-dirs```, is the value for the directory where the library files live, it is ```src``` by default.
- ```exposed-modules``` in the cabal file tells us which libraries  we are using. By default stack creates a module called 'Lib' under src. We gotta add our more values to it (in the cabal file, and in the directory) by using commas
```
exposed-modules:    Lib,
                    Palindrome,
                    Utils
```

- in the cabal file, under ```executable```, this lists out where the "executable" code is stored, i.e the 'Main'. Stack separates the logic (in Lib) from the code that is used to invoke that logic (Main.hs in ```App```)
- Stack has the opinion that the main will import the Lib, and execute its functions.



### Organize the project 

* Re-write Lib.hs and put all our business logic in it. Export the relevant functions.

* Re-write main, so that it calls the Lib module.

* Modify the cabal file.

 "You have to tell stack about any modules you’re depending on. For both your Main.hs file and your Lib.hs file, you’re using Data.Text
 
 For both your library and executable sections of palindrome-checker.cabal, you need to add the text package to the list of dependencies:

 ```
 library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                     , text

executable palindrone-checker-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , palindrone-checker
                     , text                     
.
.
.
```

Note how the pallindrome-checker-exe, i.e. the main/executable depends on "palindrone-checker", what does that mean here?

there is no module called "palindrone-checker". Or is there?

* And now we are set to build the project!

* Note, in the cabal file, under exposed modules, you are writing down ```Lib```,
the name of our module with the business logic is ```Lib```, gotta list them here so that Main can import them.

We would have to replace ```Lib``` in the cabal file, if the module in our ```src``` was called something else.



## Building and running the project

- After making changes to the cabal file, modifying the code etc, Execute: 							```stack setup``` in the directory. This uses a resolver to install a version of ghc that was used when one wrote the project. 
- The resolver is set in stack.yaml, usually ```lts-7.9``` 
-  To build the project we use ```stack build```
-  For stack, to run the program, we use the ```exec``` command and pass the name of the executable (which is defined in the \*.cabal file). Example 								```stack exec palindrone-checker-exe```


Stack feature to avoid having to re-write language pragmas in each module:
- in both the library and executable sections of the \*.cabal file. Below, the 
  ```default-language	: Haskell2010```, add the language extension, for example,
`` extensions: OverloadedStrings```



**To force stack  to use system GHC, instead of downloading an isolated GHC:**

```
stack config set system-ghc --global true
```


## Testing Haskell Projects

We discuss three types of testing.

### 1. Opening our entire project in GHCi

We wanna load our project into GHCi, so that we can manually run our functions in the CLI. 

For that, we need to first execute ```stack setup```, and ```stack build```, after which we can do a ```stack ghci```.  From which you'll be able to interact with the project's functions in ghci.

This is for manual testing

### 2. Unit Testing our methods


Implement your unit tests in ```/test/Spec.hs```, such as using ```asserts```, and then execute,
```stack test```.

The tests can look something like this,
```
main :: IO ()
main = do
  putStrLn "Running tests..."
  assert (isPalindrome "madam") "passed 'madam'" "FAIL: 'madam'"
  assert (isPalindrome "aditya") "passed 'aditya'" "FAIL: 'raadityacecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  assert ((not . isPalindrome) "random") "passed 'random'" "FAIL: 'random'"
  putStrLn "done!"
```

On executing ```stack test```,  it runs these tests,
```
Running tests...
passed 'madam'
FAIL: 'raadityacecar'
FAIL: 'racecar!'
passed 'random'
done!
```
Looking at these results, you can continue to modify the logic in Lib (adding to it, example: making sure that "racecar!'' passes as palindrome.)


### 3. Automating Unit Tests / Generating Data for Unit Tests / Testing Properties of Functions (extremely cool)

Unit testing is a way to automate manual testing of the code.
Property testing is a way to automate unit tests. 

We wanna test if the function has a certain property.

To do that we use Quickcheck.

We supply QuickCheck with a some properties that our function is supposed to uphold.
QuickCheck automatically generates values, and tests them out on the functions --- making sure that the functions uphold the properties.

Example,

for a function ```isPallindrome```, the following property must always hold true. Running ```isPallindrome``` on ```text```, should return the same value as running it on ```reverse (test)```.

So we formalize this property:
```
prop_reverseInvariant text = isPalindrome text == (isPalindrome (reverse text))
```
It takes a text, and runs ```isPallindrome``` on both the text and its reverse value.

See how this function is testing a fundamental property of isPallindrome. It's quite beautiful.

Now we give this function (property) to quick check, which can generate 1000s of test cases for us, and call test this property on all of these test cases.


Running it on 1000 test cases:
```
main :: IO ()
main = do
	   quickCheckWith stdArgs { maxSuccess = 1000}  prop_reverseInvariant
```


---

Another property that the ```isPallindrome``` function must uphold: calling this function on a string with punctuation marks must give the same result as calling it on a string without those punctuation marks. That is, it should be punctuation invariant,

property is implement as:
```
prop_punctuationInvariant2 text = isPalindrome text == isPalindrome noPuncText
    where noPuncText = filter (not . isPunctuation) text
```

Why is this so cool? Because if we have not handles punctuation invariance properly in our code, QuickCheck would find out, by having tested this property on 1000s of values.


```

main :: IO ()
main = do 
    quickCheck prop_punctuationInvariant2
    putStrLn "done!"
```


My thoughts:

these properties on functions are able to capture deep behavior of our functions. And can help us "prove"  these methods flawlessly. I think this can certainly tie to Coq, or Automated Theorem Provers. 

Maybe this has implications on formal verification of code too! 
I loved this chapter quite a lot. And for property testing, my mind is blown.


(todo: Look up property testing, and what else can be done using it. Find out how formal verification works.)



*Notes on using QuickCheck*

- QuickCheck might not be able to create values of all types. The type it targets must be an  ```Arbitrary```
- To resolve this one can install ```stack install quickcheck-instances```, to make QuickCheck work with a variety of types. 

Implement : probabilistic prime checking https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test




## Errors

- The traditional approach of throwing errors is frowned upon in Haskell, because it throws runtime errors; that a compiler can not catch. 
- Haskell does allow us to throw errors, but there are other--better--ways to handle problems that show up in code. Example using Maybe.
- ```Maybe``` type can not communicate a "lot" about what "happened." So there is another more powerful type -- ```Either```, that lets us use any value we like to communicate the error.


#### Example, The ```head``` function, that can cause runtime errors that are not caught by the GHC

Albeit useful, using ```head``` on an empty list gives us an error
```
Prelude> head []
*** Exception: Prelude.head: empty list
```


Here is the problem: if you compile a code in which head is being operated on an empty list -- GHC **wont** throw a compiler error. You'll realize this issue in runtime only. Where this blows up.

Moreover, the type signature of ```head``` gives no indication that the function will throw a compiler error
```
Prelude> :t head
head :: [a] -> a
```


#### For compiler warnings. (Example, cases where we accidentally perform ```head []```  operation)
A trick: Using the ```-Wall``` flag to run GHC,

Add the flag in your stack project, in the cabal file,
```
executable projectname-exe:
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall   
  build-depends:       base
.
.
```

If we don't want to miss any warnings, we can compile with ```-error``` flag which will convert warnings to errors.


#### What is wrong with ```head```?

```head``` is a partial function, partial functions are often not defined on all the possible inputs. ```head``` is not defined on the ```[]```.

(Claim "most errors in software are due to partial functions "Your program receives input you don’t expect, and the program has no way of dealing with it."

A scenario: a new function ```fooBar``` is created which takes some input and internally calls some other function (that maybe had lesser scope), maybe it calls a bunch of other internal functions. fooBar believes that's scope is now widened, it begins to accept inputs that the internal functions don't have a good way of dealing with. ```fooBar``` in this case is a partial function (if it calls other functions internally which are being sent arguments that ```fooBar``` opinionatedly decided on.)


#### Throwing errors
Throwing errors is an obvious solution, Haskell has an inbuilt function called ```error```. Example,
```
myHead :: [a] -> a
myHead [] = error "empty list"    -- BAD PRACTICE        
myHead (x:_) = x
```

Also, _never_ use head, instead use pattern matching. As compiler can warn us if we have pattern matching issues. 


**We need a type that can capture when errors might happen, and we need the compiler's help in writing more error-resistant code**



Note,

-   maximum, succ, sum, ```/``` are also partial functions in prelude that fail on certain inputs.  ( empty lists, maxBound , infinite lists, what does ```/``` fail on?)




### Handling partial functions with Maybe

Remember that ```Maybe``` lets us avoid the use of Null, that is used so often in other languages. ```Maybe``` can transform a partial function into a "complete" function. 

So instead of doing this,
```
myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x
```

We can do this,
```
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x
```

We can use ```<*>```  and ```<$>``` on this 
```
result1 = ( * 10 ) <$> maybeHead [10,20,30]
result2 = (*) <$> maybeHead [10,2,3]  <*> (Just 55)
exampleVal = (:) <$> maybeHead \[1,2,3\] <\*> Just \[1,2,3\]
```

Another example,
To make take safer we can do, 
```
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))
```

Although, tail is also a partial function. #CSTODO to dig into this further.


## The Either type

Although the ```Maybe``` type is very [[08-02 Handling partial functions with Maybe|useful to handle edge cases]] and exception scenarios. Returning ```Nothing``` does not convey a lot.

Example, a primality testing program, which returns ```Nothing``` if the number given to it is more than what it can compute. Here, ```Nothing``` does not convey anything, it is quite cryptic. 

So we introduce the ```Either``` type.

Either is defined,
```
data Either a b = Left a | Right b
```

This is **beautiful**, this is a type that can send the ACTUAL data, OR in case of an error, a meaningful error message. 

This is an ```or``` type, for error handling scenario we return the ```Left``` constructor. We return the ```Right``` constructor when things go as planned.
The ```Right``` constructor is similar to basically similar to ```Just```.

This is more powerful than ```Maybe``` because we can return the ```Left``` constructor _and_ it can convey an error message.  If we are operating with an ```Either``` type in a function, and the ```Left``` constructor pops up, the GHC won't forcibly try to carry out the operation with the ```Left``` constructor (I think),

example,

```
> (+12) <$> (Right 101)
Right 113

> (+12) <$> (Left 101)
Left 101

> (+12) <$> (Left "errrorrr!")
Left "errrorrr!"

```


Either can help us create a safer type of the ```head``` function,
```
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x
```

Note the type signature,   ```Either String a``` , means Left constructor takes a string, right constructor (if things go according to plan) takes ```a```, i.e. an element from the list.

Example,
```
> eitherHead [1..]
Right 1
>

> eitherHead []
Left "Can't return head -- List Empty!"
```




Also, ```Either``` implements a Monad (and thus a Functor, and Applicative as well. Why? Because Monad _is_ an Applicative, and an Applicative _is_ a functor.)
This is clear because above we apply the ```<$>``` and the ```<*>``` on the Either already. 

A more involved example, add the first two elements of a List using ```eitherHead```,

```
add2From :: [Int] -> Either String Int
add2From myList =  (+)  <$>  (eitherHead myList) <*> (eitherHead (tail myList))
```

Examples,
```
> 
> addTwoValuesList [1,2,3]
Right 3
>
> 
> addTwoValuesList [1]
Left "Can't return head -- List Empty!"
>
> 
> addTwoValuesList []
Left "Can't return head -- List Empty!"
>
>
```
Beautiful.


Remember that ```Either``` lets us use any type we want to.
Also we can stick to n number of error messages using Either.

### An example: Prime Checker with Either


Remember how we discussed earlier how ```Maybe``` is not sufficient enough to display nuanced error messages. [[08-03 The Either Type]] We discussed the nuance  required for primality checking.

Now, ```Either``` let us return a whole array of error messages. And display the relevant message (on failure) in our primality testing.


```
isPrime :: Int -> Either String Bool
isPrime n
   | n < 2 = Left "Numbers less than 2 are not candidates for primes"
   | n > maxN = Left "Value exceeds limits of prime checker"
   | otherwise = Right (n `elem` primes)
```


Testing the isPrime Method
```
*Main> isPrime (-100)
Left "Numbers less than 2 are not candidates for primes"
*Main> isPrime 17
Right True
*Main> isPrime 10
Right False
*Main> 
*Main> isPrime 1000000000
Left "Value exceeds limits of prime checker"
*Main> 
```

### Error handling with ```Either```

So far we haven't taken advantage of the fact that the ```Either``` type class can accept arbitrary data type. 

Many programming languages have custom types for various kinds of errors. We can create similar types which model an error, and pass it to ```Either``` to show the relevant errors.

Example, for primality testing errors, we can have,
```
data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
	show TooLarge = "Value exceeded max bound"
	show InvalidValue = "Value is not a valid candidate for primality checking"
```

Based on the above, we can modify our primality checking function,
```
isPrime :: Int -> Either PrimeError Bool
isPrime num | num < 2 = Left InvalidValue
		    | num > maxBound = Left TooLarge
			| otherwise = Right (elem num primes)
```

Testing it,
```
> 
> isPrime 13
Right True
> isPrime 10
Right False
> 
> isPrime (-1)
Left Value is not a valid candidate for primality checking
> 
> isPrime 0
Left Value is not a valid candidate for primality checking
> 
> isPrime 10000000
Left Value exceeded max bound
> 
```


Makes code much more readable. And returns valid information. As Jason Fried says, "even the error messages in your software are a form of marketing."


Right now our primality checker is returning a ```Either PrimeError Bool``` type, why not return the client a ```String``` always?

```
displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "Number is a prime"
displayResult (Right False) = "Number is a composite"
displayResult (Left primeError) = show primeError --can take Eithers out of ctx
```

A main IO action,
```
main :: IO ()
main = do   
        input <- getLine
        let inputInt = read input :: Int -- or use, input <- read <$> getLine
        let primeTestResults =  (displayResult . isPrime) inputInt
        putStrLn primeTestResults
        main
```

Its testing,
```
> main
1
Value is not a valid candidate for primality checking
0
Value is not a valid candidate for primality checking
1000
Value exceeded max bound
25
Number is a composite
13
Number is a prime
4
Number is a composite
.
.
.
```


Thus, creation  of error classes (like ```PrimeError```) demonstrates sophisticated ways of handling errors. 

 Because of the flexibility of the ```Either``` type, i.e how the ```Left``` constructor can _be_ any type, the expressiveness is huge. As we can provide the ```Left``` data constructor types like Strings, or Ints, other custom error classes, or even a function. In which case the ```Left``` would return a function. 
 
In summary, ```Either``` has a dual function, it can  help us safely handle errors -- providing detailed information about it. It also lets us return actual data (just like a ```Maybe```)

 

## HTTP 

We use the Network.HTTP.Simple library for HTTP. This library is part of the http-conduit package.

The library makes it easy to make simple HTTP requests.

Generally we create an instance of the  ```Request```  data type, and pass it to ```httpLBS``` to execute. 

Example, 
```
> response = httpLBS "http://news.ycombinator.com"
```

Just Defining a request object does not execute the HTTP Request (because of lazy evaluation)

The response object is actually wrapped in an ```IO```.  It's type is 
```IO (Response a)``` . More specifically ```IO (Response LC.ByteString)```

To get the status from the response,
```
> getResponseStatusCode <$> response
```

An alternative solution is using ```<-```, which lets us take the value out of context (even inside GHCi). Example,
```
Prelude> response <- httpLBS "http://news.ycombinator.com"
Prelude> getResponseStatusCode response
200
```

### Creating an HTTP GET Request

We've got to 
- add token to our request
- specify the host and path
- use GET
- make sure request works for "SSL Connection"

---

Note, ```defaultRequest``` is provided by the library.

Note also, the type signature of these setters,
```
> :t setRequestPort
setRequestPort :: Int -> Request -> Request
```
They take in the "state" i.e. Request, and return a new Request object.
(Author W. Kurt says, "Here you see one functional solution to having state. You create a new copy with the modified value" )

Using the relevant functions we build our request,
```
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
                    -> BC.ByteString -> Request
buildRequest token host method path =  setRequestMethod method 
                                        (setRequestHost host
                                            (setRequestHeader "token" [myToken]
                                            (setRequestPath path
                                                (setRequestSecure True
                                                        (setRequestPort 443 defaultRequest )))))
```

This is super-cumbersome, so we use the syntactic sugar via ```($)``` operator

```
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token]
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest
```



To execute this request,
- pass ```Request``` to ```httpLBS```
- check if the status is 200.
- if it's 200, use ```getResponseBody``` and write the response to a file (note: you must use lazy ByteStrings - L.writeFile, instead of the function in Char8)
- If there is an error, raise an alert. 

```
main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
         print "saving request to file"
         let jsonBody = getResponseBody response
         L.writeFile "data.json" jsonBody
    else print "request failed with error"
```

note how we can have a do block inside another do block.

Full documentation at  https://haskell-lang.org/library/http-client 


## JSON: Basic concepts


We use the library ```Data.Aeson```  to work with json. It allows us to translate between a haskell type and json.

We use two methods to do that,
- **```decode```** whose type signature look likes,
```
decode :: FromJSON a => ByteString -> Maybe a
```
Note how the type we want to convert to json should be a ```FromJSON``` type. That is, it must implement certain methods to let us convert a (json) ByteString into the type. The function returns the type wrapped in a ```Maybe``` because: parsing the json (before we can translate it into haskell object) might throw a parse error.

- Similarly, we have **```encode```. **
```
encode :: ToJSON a => a -> ByteString
```
The data type to encode must be a ```ToJSON```.


We also, have ```eitherDecode``` which gives us a more detailed error statement in case there is failure to parse the JSON.



## Converting Objects ToJSON or FromJSON: via language extension DeriveGeneric

We use the language extension ```DeriveGeneric```, and make our object derive  from ```Generic```, example
```
import GHC.Generics

data Book = Book
            { title :: T.Text
            , author :: T.Text
            , year :: Int
            } deriving (Show,Generic)
```

Then we declare this type an instance of ```FromJSON``` and ```ToJSON```, example:
```
instance FromJSON Book
instance ToJSON Book
```
And now we will be able to convert Book back and forth from a json bytestring.

Example,
```
myBook :: Book
myBook = Book {author="Will Kurt"
              ,title="Learn Haskell"
              ,year=2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook
```

and,
```
rawJSON :: BC.ByteString
rawJSON = "{\"author\": \"Will Kurt\",\"title\": \"Learn Haskell\",\"year\": 		                        												2017 }"
bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON
```


*I genuinely don't understand how deriving from ```Generic``` can let us automatically make our type a ```FromJSON``` or ```ToJSON```*


## Write our own instances: FromJSON or ToJSON by hand

### FromJSON (decode)

Making our type an instance of FromJSON. 

We have a json, ```decode``` it into a haskell object.
```
decode :: FromJSON a => ByteString -> Maybe a
```


Let's say we get hold of a JSON from an external source (say a GET request), example,
```
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"
```

We need to model the above json with our Haskell type,
```
data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , error :: Int               
                    } deriving Show
```
As we know, that the above syntax creates two functions                                       
```message :: ErrorMessage -> T.Text``` and
```error :: ErrorMessage  -> Int```

So this causes a conflict because ```error``` is already defined in ```GHC.Err```.

**So we change the name of the field**
```
data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , errorCode :: Int
                   } deriving Show
```

Now if we try to automatically make ```ErrorMessage``` derive  ```ToJSON``` and ```FromJSON```, we run into another problem because the ```decode``` function now expects an "error" field (in our ```ErrorMessage```), but does not find one.
```
decode :: FromJSON a => ByteString -> Maybe a
```


**To make our ```ErrorMessage``` type an instance of ```FromJSON```, we need to implement the ```parseJSON``` function for ```ErrorMessage```**

```
instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"
```

Here
- ```(Object v)``` is the JSON object.
- ```ErrorMessage``` is the function ``` T.Text -> Int -> ErrorMessage```
- ```v .: "message"``` a value in context. From the JSON Object v it is _parsing_ the "message" field, returning its value in a context.
- ```v .: "error",``` again, looks into the JSON object, and retrieve the key associated with "error". 

And basically, extracting the values from the JSON, and giving them to the ```ErrorMessage``` is how we implement parseJSON for ```ErrorMessage```.

For more clarity the type signature is,
```
(.:) :: FromJSON a => Object -> Text -> Parser a
```


#### Examples,
```
data Name = Name
    { firstName :: T.Text
    , lastName :: T.Text
    } deriving (Show)


instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "firstName"
                                <*> v .: "lastName"
								


-- Converting,

sampleAdi :: BC.ByteString
sampleAdi =  "{\"lastName\":\"Verma\",\"firstName\":\"Aditya\"}"

adiInAConextNow :: Maybe Name
adiInAConextNow = decode ( sampleAdi)
```

Hence the json was converted to a Haskell type.
```
Prelude > adiInAConextNow 
Just (Name {firstName = "Aditya", lastName = "Verma"})
```


### ToJSON (encode)

```
encode :: ToJSON a => a -> ByteString
```

We have a Haskell object, we wanna convert it to JSON. 
We need to implement the method ```toJSON```.


For ```ErrorMessage``` it looks like,

```
instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
           , "error" .= errorCode
           ]
```

The object function takes a ```Pair``` and returns a ```Value``` (a json object?), these are types defined in ```Aeson```.

The operator  ```(.=)``` is used to create a key/value pair matching the value of your data (message/errorCode) with the field name for the JSON object (message/error).

#### Example
```
message = "hi" 
error = "err"
obj = object [ "message" .= message, "error" .= error]
```

Where obj is
```
> obj
Object (fromList [("error",String "err"),("message",String "hi")])
```
This is how  this object functions helps  ```toJSON```  convert our type into a JSON object.

Encode objects of ```ErrorMessage``` type. Example
```
getMessage :: ErrorMessage
getMessage = ErrorMessage "Random Message" 200
```

Testing,
```
> encode getMessage
"{\"error\":200,\"message\":\"Random Message\"}"
```

Other examples,
```
instance ToJSON Name where 
    toJSON (Name firstName lastName ) = object [  "firstName" .= firstName, 
                                                "lastName" .= lastName]

adi = Name "Aditya" "Verma"
adiJSON = encode adi

-- gives us:
-- "{\"lastName\":\"Verma\",\"firstName\":\"Aditya\"}"
```



More info here: https://artyom.me/aeson

---

# Databases

(for code. go to the directory 47db/* )

## Setting up the db
We use the ```sqllite-simple```  to interact with the db. 

Create you tables, add records in them, all in an sql file say ```build_db.sql```, then execute,
```
sqlite3 tools.db < build_db.sql
```
Now ```tools.db``` is our db name, a new file will be generated in our directory. To load the db, execute,
```
sqlite3 tools.db
```
To test it out,
```
sqlite> select * from tools;
1|hammer|hits stuff|2017-01-01|0
2|saw|cuts stuff|2017-01-01|0
```

## Adding records to the DB via Haskell

We added values into the table via raw SQL, like
```
INSERT INTO users (username) VALUES ('aditya');
```

How do we do this in Haskell? 
- Establish a conn with the db. ```conn <- open "tools.db" ```
- Use the ```execute``` function which allows us to insert values into the table, Example 
  ```
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName) 	
  ```
   Note here that the ```(?)``` lets us safely pass values into our string. Only is used to create _single element_ tuples.  "This is needed because ```execute``` expects us to pass a tuple of certain size for our values.
- We also need to **Close** the connection ```close conn```
- We wrap this up in a ```do``` block in an IO action

```
addUser :: String -> IO ()
addUser username =  do  
                    conn <- open "tools.db"
                    execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
                    print "user added"
                    close conn
```

### Creating a general method which handles the connection to our db and performs an IO action.
```
-- takes in the db value , and an IO action
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName operation = do
                            conn <- open dbName
                            operation conn 
                            close conn
```

The above function you can use to avoid having to deal with ```conn```,
```
addUserNew :: String -> IO ()
addUserNew user = withConn "tools.db" $
  (\ conn -> do 
             execute conn  "INSERT INTO users (username) VALUES (?)"(Only user)
             putStrLn "added user")
									
```

	

For more brevity we can do this:
```
-- lambdas are tedious, so we create,
executeWrapper :: ToRow q =>  Query -> q -> String -> (Connection -> IO ())
executeWrapper sqlStml tuples successMsg =
            (\ conn ->  do 
                        execute conn sqlStml tuples
                        putStrLn successMsg
            )
```

Now we can skip the lambdas and the ```conn``` in our db operations, example,
```
-- now,
addUserNew2 :: String -> IO ()
addUserNew2 user = withConn "tools.db" $
     				executeWrapper "INSERT INTO users (username) VALUES (?)" 						 
					                 (Only user)
											"added the user"


-- and, a function to checkout a tool
checkoutMy :: Int -> Int -> IO ()
checkoutMy userId toolId = withConn "tools.db" $
        executeWrapper  "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" 
                             (userId,toolId) 
		                        "Checked out the tool"
````


##  Reading from a DB

The challenge here is that we want to convert rows of the DB into Haskell types. For this ```sqlite-simple``` library offers a function called ```FromRow```. 

If we wanna convert rows in a database table into a Haskell type ```a```, the type must implement ```FromRow```, which is done by implementing a function ```fromRow```. Definition of ```FromRow```
```
class FromRow a where
   fromRow :: RowParser a
```
Consequently, we will be able to transform queries into lists of our datatype.

## Making our data an instance of ```FromRow```

We have to tell the ```RowParser``` how to construct our data type.  We use the ```field``` function (implemented in SQLite.Simple) which consumes the data in the table's row and transforms it into values required by our type constructor. 

To implement ```fromRow``` for ```User``` and ```Tool```,
```
instance FromRow User where
	fromRow = User <$> field
	               <*> field

instance FromRow Tool where
	fromRow = Tool <$> field
				   <*> field
				   <*> field
				   <*> field
				   <*> field
```
Now that both ```User``` and ```Tool``` are instances of ```FromRow``` we can execute queries and translate their results directly to haskell types.

## Querying the DB
We use these two methods to query,
```
query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
query_ :: FromRow r => Connection -> Query -> IO [r]
```
The difference is ```query_``` is for queries that take no arguments.

To retrieve the set of users,
```
printUsers :: IO ()
printUsers = do 
        withConn "tools.db" $
                (\ conn ->
                           do
                           values <- query_ conn 
						            "SELECT * FROM users"  :: IO [User]    
                           mapM_ print values          
                )                           
```

### More query examples, and a meta query executor

We can write a general function which can take a query and execute it,
```
-- can execute all queries returning a tool
printToolQuery :: Query -> IO ()
printToolQuery query = withConn "tools.db" $
                        ( \ conn -> 
                                    do
                                    values <- query_ conn query :: IO [Tool]
                                    mapM_ print values)
```

Example use,
```
> printToolQuery "Select * from tools"
1.) hammer
 description: hits stuff
 last returned: 2021-01-01
 times borrowed: 0

2.) saw
 description: cuts stuff
 last returned: 2021-01-01
 times borrowed: 0
 ```
									
## Updating records

To update records we need to
- Retrieve the record, and convert it to a haskell object
- Update the object with the new values
- Run an ```UPDATE``` query via an IO Action to update the record
- Write a function which calls the functions above, i.e. orchestrating the whole thing


## Retrieve the record, and convert it to a haskell object
```
-- first obtain the tool
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
   resp <- query conn
           "SELECT * FROM tools WHERE id = (?)"
           (Only toolId) :: IO [Tool]
   return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x
```

## Update the object with the new values
```
-- then update the tool (the Haskell type)
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
   { lastReturned = date
   , timesBorrowed = 1 + timesBorrowed tool
   }
```

## Put the object in the DB (update query)
```
-- then put this new tool values in the DB,
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =  withConn "tools.db" $
                 \conn -> do
                          let q = mconcat ["UPDATE TOOLS SET  "
                                              ,"lastReturned = ?,"
                                              ," timesBorrowed = ? "
                                              ,"WHERE ID = ?;"]

                                  execute conn q (lastReturned tool
                                             , timesBorrowed tool
                                             , toolId tool)
                              
							      print "tool updated"
```


## Putting all these methods together
```
updateToolTable toolId = do
                         conn <- open "tools.db"
                         chosenTool <- selectTool conn toolId
                         currentDate <-  utctDay <$> getCurrentTime  
                         let newTool = (fmap updateTool chosenTool) <*> 
						 						(pure currentDate) 
						 
								-- we need a Maybe type for date ^
								-- don't use Just, use pure to convert

                         -- nowt hat we have our updated tool,
                         -- we can use, updateOrWarn ::  Maybe Tool -> IO ()
                         -- newTool is a MaybeTool, 
						 -- and updateOrWarn takes just that as a parameter
						 
                         updateOrWarn newTool
                         close conn
```						 


### Deleting records,

```
checkin :: Int -> IO ()
checkin toolId =  withConn "tools.db" $
                     \conn -> do
                       execute conn
                         "DELETE FROM checkedout WHERE tool_id = (?);"
                         (Only toolId)
```

# Arrays in Haskell: UArray

There is a context for performing mutation on an array by using the ```STUArray``` type. Basically, _stateful_ mutation. 

First we look at ```UArray```, it is a non-lazy Array.

Lazy evaluation can get real inefficient. If you're performing operations (using map) on a List: haskell won't compute the values until absolutely needed, but it will _store_ all sorts (what all?) data on the computations that _might_ be done in the future.
And on doing these computation, it will take a longer because lists (lazy evaluation) are linked lists.

## Creating a UArray
To use the UArray type class, we ```import Data.Array.Unboxed```. (And add ```array``` to the list of build-depends if we're using stack.)

It has the kind ```UArray :: * -> * -> *```, the two other types it accepts are 
- the first, type of the index. Must be an Enum and Bounded. Example, Char, Int, Bool
- the second,  is for the type of the value.
Example, ```zeroIndexArray :: UArray Int Bool```

To create a UArray, we use the ```array``` fxn. Takes two arguments. One, is a tuple ```(lowerbound, upperbound)```. Second, the list of  values you wanna put in your array ```[(index,value)]```. 
```
> :t array 
array :: (IArray a e, Ix i) => (i, i) -> [(i, e)] -> a i e
```
For values that you don't explicitly add to your array. Haskell will put in a default value.

To **look up values** in your UArray by using the ! operator,
```
n> zeroIndexArray ! 1
True
*Main> zeroIndexArray ! 2
False
*Main> zeroIndexArray ! 3
False
```

Creating an array whose index starts from 1 is trivial,
```
oneIndexArray :: Int ->  UArray Int Int
oneIndexArray up = array (1,up) $ zip [1..up] [1..up]

test1 = oneIndexArray 5
```


## Updating the UArray
Array is updated like other functional data structures. By creating a copy of the original array with the modified values.

```
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) [] -- initializes everything to zero
```

We use the ```//``` operator to modify a UArray, example,
```
newArray = beansInBuckets // [(1,9),(3,11)]
```
Now, ```newArray``` equals ```array (0,3) [(0,0),(1,9),(2,0),(3,11)]```.


### Modify _all_ elements of the UArray
We use a function called ```accum``` defined in ```Data.Array.Base```. This function takes in three arguments
- a binary operation
- a UArray
- an indexed list of values to apply to the UArray to ```[(0,1),(0,2),(0,3)]``` etc

Add 1000 to each element in our UArray,
```
newArray1000 = accum (+) updatedArray $ zip [0 .. 3] $ cycle [1000]
```



# STUArray


Efficient array algorithms _require_ us to change state of the array. 

```STUArray``` is a special type of ```UArray```. "The STUArray uses a more general type called ST. ST allows for stateful, nonlazy programming." STUArrays are a type of monad. STUArray lets us change values in a UArray.

To use STUArrays, we import
```
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
```

In a STUArray context, you can perform stateful computations.  Although this is **not** a hack that will let us disregard FP principles. We can only use STUArray only when the _statefulness_ is indistinguishable from pure functional code for the "users" of our functions.

We implement a function ```listToSTUArray```, that takes a list of Ints and transforms the list into an STUArray.

To help us with that,  we will use the ```writeArray``` function, which takes an STUArray, an index, and a value. This is the **crux**. ```writeArray``` performs a stateful mutation of the underlying array _without creating a copy of the array!_

We also use ```newArray``` function, which takes a pair representing the bounds of the array as well as a value for initializing the array. This returns an empty STUArray the specified size. 

```
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do             
      let val = vals !! i                   
      writeArray myArray i val              
    return myArray
```
- In ```newArray (0,end) 0``` we are initializing the empty STUArray.
- Next in a ```forM_``` we extract values from ```vals```, and then write them to our STUArray ```myArray``` using the function ```writeArray```.
- In the end, we return this the array back to its context.

On trying to print the STUArray, we get
```
GHCi> listToSTUArray [1,2,3]
<<ST action>>
```


## Taking STUArrays out of the context
The ```ST``` context is much safer than ```IO```, as referential integrity still holds. So, haskell lets us take these arrays out of the context, via
```
runSTUArray :: ST s (STUArray s i e) -> UArray i e
```

Example,
```
> runSTUArray ( listToSTUArray [1,2,3] )
array (0,2) [(0,1),(1,2),(2,3)]
```

We can create a wrapper,
```
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals
```

Quote, "STUArray forces us to maintain perfect encapsulation, we can leave the context of the STUArray without violating any of the core rules of functional programming"



We can combine ```runSTUArray``` with our original function,
```
listToUArray_ :: [Int] -> UArray Int Int
listToUArray_ vals = runSTUArray $ do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray
```


## Bubble Sort using STUArray 

How do we use UArray in the context of an STUArray?

- We use a function called ```thaw``` which will unfreeze our UArray.
- We use ```bounds``` fxn, which will give us the bounds of our array.
- STUArray has a function called readArray that reads a stateful value from an array.


```
-- author's implementation
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
   stArray <- thaw myArray                             
   let end = (snd . bounds) myArray                    
   forM_ [1 .. end] $ \i -> do
     forM_ [0 .. (end - i)] $ \j -> do
       val <- readArray stArray j                      
       nextVal <- readArray stArray (j + 1)
       let outOfOrder = val > nextVal
       when outOfOrder $ do                            
         writeArray stArray j nextVal
         writeArray stArray (j + 1) val
   return stArray  
```

```
-- my implementation, 
bubbleSort :: [Int] -> UArray Int Int
bubbleSort vals = runSTUArray $  do
                  let end =  length vals - 1
                  stuArray <- listToSTUArray vals
                  forM_ [0..end] $ \ j -> do 
                        forM_ [0..(end-1)] $  \ i -> do 
                            val1 <- readArray stuArray i
                            val2 <- readArray stuArray (i+1)
                            if (val1 > val2) then 
                                do
                                writeArray stuArray i val2
                                writeArray stuArray (i+1) val1
                            else return ()    
                  return stuArray  
```

We are maintaining perfect "encapsulation", even though we are changing state, it is not apparent to the external world.
Because we can translate our stateful data structure STUArray back to a regular UArray. This lets us treat stateful code as pure functions.

#  Monads, revisited


"...three properties, and a few rules about how we can use them together, that define a monad in Haskell. Let's revisit the above list in condensed form.

1. A type constructor m.

2. A function of type m a -> (a -> m b) -> m b for chaining the output of one function into the input of another. No comments

3. A function of type a -> m a for injecting a normal value into the chain, i.e. it wraps a type a with the type constructor m.

## Implementation of ```>>``

The ```>>``` function,

```
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f
```

Same as (>>=) except it ignores the output from ```a```. 


# Parsec

* myParser :: GenParser Char st [String]

When definning a GenParser, the above type signature means, we are accepting a sequence of characters, 
and returning a list of strings.


All these miniparsers are bascially "splitting" input strings based on various parameters.

Check the comments on ```49parsec.hs``` for theory.

Example,

```
x = char ',' :: GenParser Char st Char
```
Now,
```
> parse x "?" ",,,,,,."
Right ','

> parse x "?" "ABC"
Left "?" (line 1, column 1):
unexpected "A"
expecting ","
```
parse returns an either, the second example does not find a ```,``` and returns a Left Error. The firsr example finds a ```,``` and says that the parsing was successful ```Right ','```


* The ```many``` function,
```
cell = many (noneOf ",\n") 
```

```cell``` parses input till it encounters a "," or "\n"

Many takes a parsing function, and applies it over and over again.
noneOf is a parser which parses anything except for ```,``` and ```\n```

the resulting ```cell``` parser will parse till it finds the ',' or '\n', and then returns everything it found before the ',' or '\n'



## The sepBy and endBy Combinators

* ```sepBy``` function

The function takes two arguments. One, is a parser that can parse "some sort of content". Two, is another parser that parses for a separator.

It tries to parse content, and then tries to parse a separator, and then back and forth; until it cant parse a separator. And returns a list of content that it was able to parse.

* ```endBy``` function

this is same as ```sepBy```, except it expects that last element to be followed by a separator. 
That is, it continues parsing content until it can't parse any more content.

We use "endBy" to parse lines, since every line must end with the end-of-line character. 

Due to these both functions,
the parser can be written more succintly,

```
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n") 
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
```

Testing,
```
> parseCSV "1,2,3\n4,5,6\n7,8,9\n"
Right [["1","2","3"],["4","5","6"],["7","8","9"]]
```


From the book,
* A  ```csvFile``` contains 0 or more lines,  
each of which is terminated by the end-of-line character.

* A ```line``` contains 1 or more cells, separated by a comma. 

* A ```cell``` contains 0 or more characters, which must be neither the comma nor the end-of-line character. 

* The end-of-line character is the newline, ```\n```


You can give all these mini parsers to the ```parse``` function and test them out,

```
>parse eol "(unknown)" "blahblah\n"
Left "(unknown)" (line 1, column 1):
unexpected "b"
expecting "\n"

> parse eol "(unknown)" "\n"
Right '\n'
>
```
## Choices and Errors

