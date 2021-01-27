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
