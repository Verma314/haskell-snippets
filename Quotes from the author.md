Quotes from the author

* * From the author,
" All of these type classes represent the design patterns of functional programming
.. 
Both OOP design patterns and category theoretic type classes abstract out common programming patterns. The only difference is that Haskell’s are based on mathematical foundations, rather than ad hoc patterns discovered in code. Just as Haskell’s functions derive power from their mathematical basis, so do Haskell’s design patterns."


* The careful reader may have noticed something strange about Map being a Functor. Map’s kind is * -> * -> * because Map takes two type arguments, one for its keys and another for its values. Earlier we said that Functors must be of kind * -> *, so how can this be? If you look at the behavior of <$> on your partsDB, it becomes clear. Functor for Map is concerned only about the Map’s values and not its keys. When Map is made an instance of Functor, you’re concerned only about a single type variable, the one used for its values. So for the purposes of Map being a member of Functor, you treat it as being of kind * -> *.


* As you can see, Functor’s <$> provides a common interface to apply any function to a value in a context. For types such as List and Map, this is a convenient way to update values in these containers. For IO, it’s essential to be able to change values in an IO context, because you can’t take IO values out of their context.