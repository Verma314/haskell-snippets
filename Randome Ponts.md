* Looping-ish :P
```
import Control.Monad
forM_ [1,2,3,4,5] print
```

Search Control.Monad, revisit what ```mapM_``` does

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