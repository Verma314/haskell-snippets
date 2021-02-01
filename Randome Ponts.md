* Use ```pure```

Example,
```
   userId <- pure read <*> getLine
```
Don't use (read :: Int),

or in Maybe context (Just value) if you wanna put something in a context


* "When you use >>=, you’re passing an argument in a context; the >> operator is used when you want to chain together actions and disregard their output." 

* if you wanna print  (ie ```Show```) random types to the screen -- Use ```print```,
not ```putStrLn```


* Creating AND data types and avoiding parentheses,

```
data Sum = Cons Int Sum | EmptyList deriving (Show)
```

Test,
```
> x = Cons 1 $ Cons 2 $ Cons 3 $ EmptyList 
> x
Cons 1 (Cons 2 (Cons 3 EmptyList))
```


Converting it to json,
```

instance ToJSON IntList where 
    toJSON (Cons element restOfTheList ) = 
              if (restOfTheList == EmptyList ) 
              then  
              object [ "Element: " .= element ] 
              else object [ "Element: " .= element, " $ " .= toJSON restOfTheList]

jsonedList = encode x
```


* Why use mconcat instead of ```++```?

 mconcat works on all major string types: String, Text, and ByteString. 
This makes refactoring code to change types as easy as changing type signatures.


* Looping-ish :P
```
import Control.Monad
forM_ [1,2,3,4,5] print
```

Search Control.Monad, revisit what ```mapM_``` does


mapM_ takes two argume

```
> x = mapM_ (putStrLn . show .(+ 1) . read )  ["1","2"]
> :t x
x :: IO ()
> x
2
3
```

* A more robust way to force stack to use PATH ghc is to modify the stack.yaml, and insert your ghc version,
```
resolver: ghc-8.8.4
system-ghc: true
```

Make sure this is uncommented
```
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/17/0.yaml
```


Is this a bug in stack? When we comment the resolver, the ```stack build``` does not work. It just hangs at ```Downloading Index```.
Should this be fixed?

CSTODO: Study a bit more about resolver and stack, and then understand why this is being caused, reproduce this issue, is this _is_ an issue -- raise an issue with the stack repo.

* Maybe is an instance of Monad (and therefore Functor and Applicative),

* unfortunately, tail is also a partial function. How? Why? How was it implemented? Where does it fail?

* to read something as int, ```input <- read <$> getLine```

* Note the ```$``` operator,
it can be used to avoid parentheses, when you put the ```$``` it means open parentheses, the parentheses ends at the end of the function definition,
example
```
> (* 2) $ 2 + 2 
8
```
It's type signature can be obtained:
```
($) :: (a -> b) -> a -> b
```
it takes two arguments, a function and a value, it applies the value to the function, and returns the value.


Another example,
```
> head $ map (++"!") ["dog","cat"]
"dog!"
```

This operator has lower precendence than other operators and hence it is like the other arguments are in parentheses.


*  To set resolver while creating project. You can also do,
```
stack new projectname --resolver=lts-3.11
```

More info on stackage, resolver etc:
https://stackoverflow.com/questions/33446558/understanding-haskells-stack-program-and-the-resolver-and-lts-version/33447434


* Arrays,
To **look up values** in your UArray by using the ! operator,
```
n> zeroIndexArray ! 1
True
*Main> zeroIndexArray ! 2
```


* Modifying a UArray, 

```
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) [] -- initializes everything to zero
```

We  use the  // operator to update values in a UArray
```
newArray = beansInBuckets // [(1,9),(3,11)]
```
