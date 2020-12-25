# haskell-snippets

## One liners 

* Let and Where are both ways of creating variables. Choosing to use let or where is a matter of style the vast majority of the time in Haskell. 
* Lambdas can also be used to 'implement' variables example:
```
myFunction x y = (\ x y -> if x > y then x + y else x) (x ^ 2) (y ^ 2)
```
* "We are passing in a function and returning a lambda function. The function func that we passed in is captured inside the lambda function. When we capture a value inside a lambda function, this is referred to as a closure (on the function func). (on the u" (From Will Kurt's Get Programming in Haskell book)
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
-- here, "a" is a type of class Num
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

### Important + Interesting: Using these Type Classes 

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

Make sure that all instances of Die (ie who ever choses to be a type of Die ) then are also implementing Eq and Enum. (are also Enum and Eq themselves.)

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
#### (from Get Programming in Haskell by Will Kurt.)

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

