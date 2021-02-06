Quotes from Real World Haskell,

### Jargon on Monads

“Monadic” simply means “pertaining to monads”. A monadic type is an instance of the Monad typeclass; a monadic value has a monadic type. 

When we say that a type “is a monad”, this is really a shorthand way of saying that it's an instance of the Monad typeclass. Being an instance of Monad gives us the necessary monadic triple of type constructor, injection function, and chaining function. 

In the same way, a reference to “the Foo monad” implies that we're talking about the type named Foo, and that it's an instance of Monad.

An “action” is another name for a monadic value. This use of the word probably originated with the introduction of monads for I/O, where a monadic value like print "foo" can have an observable side effect. A function with a monadic return type might also be referred to as an action, though this is a little less common.











Quotes from Get Programming in Haskell,

* " In practice, perfect encapsulation is the same as referential transparency."

* "One important thing to understand about STUArrays and the ST type in general is that they aren’t a hack that allows you to disregard all the functional purity you’ve worked so hard for. STUArray exists to allow you to perform stateful programming only when that statefulness is indistinguishable from pure code for the users of your functions."

* "When you write your file, it’s important to notice that you’re using the raw lazy ByteStrings with L.writeFile rather than the Char8 version LC.writeFile. In lesson 25, we mentioned that when you use binary data that may include Unicode, you should never write it using the Char8 interface, as it can corrupt your data."


* * From the author,
" All of these type classes represent the design patterns of functional programming
.. 
Both OOP design patterns and category theoretic type classes abstract out common programming patterns. The only difference is that Haskell’s are based on mathematical foundations, rather than ad hoc patterns discovered in code. Just as Haskell’s functions derive power from their mathematical basis, so do Haskell’s design patterns."


* The careful reader may have noticed something strange about Map being a Functor. Map’s kind is * -> * -> * because Map takes two type arguments, one for its keys and another for its values. Earlier we said that Functors must be of kind * -> *, so how can this be? If you look at the behavior of <$> on your partsDB, it becomes clear. Functor for Map is concerned only about the Map’s values and not its keys. When Map is made an instance of Functor, you’re concerned only about a single type variable, the one used for its values. So for the purposes of Map being a member of Functor, you treat it as being of kind * -> *.


* As you can see, Functor’s <$> provides a common interface to apply any function to a value in a context. For types such as List and Map, this is a convenient way to update values in these containers. For IO, it’s essential to be able to change values in an IO context, because you can’t take IO values out of their context.


* " The great thing about Either is that because the Left constructor can be any type, there’s no limit to how expressive you can be. If you wanted to, you could return a function!"

* "Also notice that Query is its own type. You’ve been treating your queries as strings, but this is all thanks to the OverloadedStrings extension, which is automatically translating for you."


* **The ToRow type class**
You can also use the ToRow type class. But ToRow is much less useful, because it transforms your data types into a tuple of values. As you can see from our examples of creating and updating values, you either don’t have all the information you need (in the case of creating) or need only a specific subset. For reference, here’s how to make Tool an instance of ToRow (note that you need to import Data.Text as T):

```
instance ToRow Tool where
   toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
                , SQLText $ T.pack $ name tool
                , SQLText $ T.pack $ description tool
                , SQLText $ T.pack $ show $ lastReturned tool
                , SQLInteger $ fromIntegral $ timesBorrowed tool ]
```
                
The SQLText and SQLInteger constructors transform Haskell Text and Integer types to SQL data. In practice, you’ll likely use ToRow much less often than FromRow. Still, it’s good to know it exists.

*  **The ST type**
The ST type generalizes the behavior you see in STUArray. The STUArray type relies primarily on three actions: newArray, readArray, and writeArray. For the ST type, these are replaced with the more general functions: newSTRef, readSTRef, and writeSTRef. Likewise, instead of runSTUArray, you use runST. Here’s a simple example of a swapST function that statefully swaps the values of two variables in a 2-tuple:

```
swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
   x' <- newSTRef x
   y' <- newSTRef y
   writeSTRef x' y
   writeSTRef y' x
   xfinal <- readSTRef x'
   yfinal <- readSTRef y'
   return (xfinal,yfinal)
```

Just as with STUArray, the primary purpose of all ST types is to allow you to implement perfectly encapsulated, stateful computation.