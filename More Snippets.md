(you can also see 'Randome Ponts.md' for more snippets)


1. What does the maybe function do?

Consider that you wanna apply a normal function to a value in the Maybe context.
```
> fmap (+1) (Just 1)
Just 2
```
What if the function failed ie throws an exception?

The maybe seems to provide a failsafe mechanism, that makes sure a default value is returned if the function throws an exception. Successful example:
```
> maybe (10) (+ 1 ) (Just 1)
Just 2
```
Had the function thrown an error, we'd have gotten Just 10 as the output.