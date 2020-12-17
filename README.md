# haskell-snippets

### One liners 

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