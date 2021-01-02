# haskell-snippets

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
   let statement = helloPerson name                     `
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


