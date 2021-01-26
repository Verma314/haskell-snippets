* Maybe is an instance of Monad (and therefore Functor and Applicative),

* unfortunately, tail is also a partial function. How? Why? How was it implemented? Where does it fail?

* to read something as int, ```input <- read <$> getLine```