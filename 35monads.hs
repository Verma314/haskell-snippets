{-
How would you write a single IO action that 
reads in a number from the user, 
doubles it, 
and prints that value back to the user, 
without using do-notation?
-}

-- revision:
challenge :: IO ()
challenge = do
    j <- read <$> getLine  -- read converts IO String, to IO Int (via type infrence). <- is used because an IO type is returned
    let mysum = j * 2 -- let is used because IO is not returned
    let statement = show mysum
    putStrLn statement

-- Combine readInt and printDouble (defined next) into a single IO action:
readInt :: IO Int
readInt = read <$> getLine
printDouble :: Int -> IO ()
printDouble n = print (n*2)

readAndPrintDub = readInt >>= printDouble



{-Turn (+ 2) from type Num a => a -> a 

to type Num a => a -> IO a using a lambda and return.

 Use :t in GHCi to double-check that you’re getting the correct type.-}

plus2 = (+ 2)

plus2' :: Integer -> IO Integer
plus2' = ( \ x -> return (plus2 x))



--- 

-- read a string as an int
y :: IO Int
y = read <$> getLine


{-To demonstrate how you tie all these together, 
let’s write a simple IO action that will ask a user’s name, 
and then print out "Hello, <NAME>!".-}


helloName name = "Hello " ++ name ++ "!"

helloNameIO :: String -> IO String
helloNameIO = (\x -> return (helloName x))


--   (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b

soln = (>>) (putStrLn "Enter name") (getLine >>= helloNameIO)


-- Exercises:

{-
To prove that Monad is strictly more powerful than Functor, 

write a universal version of <$>, as in the preceding lesson’s exercise, 

called allFmapM, that defines <$> for all members of the Monad type class. 
Because it works for all instances of Monad, 
the only functions you can use are the methods required by the Monad type class (and lambda functions). 

To get you started, here’s your type signature:

allFmapM :: Monad m => (a -> b) -> m a -> m b


-- (>>=) :: m a -> (a -> m b) -> m b
-}


allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func typeInAContext = (>>=) typeInAContext ( \ x -> return (func x))


{-
To prove that Monad is strictly more powerful than Applicative,

write a universal version of <*>, called allApp,
that defines <*> for all members of the Monad type class. 

Because it works for all instances of Monad, 
the only functions you can use are the methods required by the Monad type class 
(and lambda functions). 

To get you started, here’s your type signature:
allApp :: Monad m => m (a -> b) -> m a -> m b

Remember,

(>>=) :: Monad m => m a -> (a -> m b) -> m b

(<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b

(target) :: Monad m => m (a -> b) -> m a -> m b

Try to think exclusively in terms of the type signatures.
Use <$> if you want and replace it with your answer to Q29.1

----
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
-}


-- not sure how to implement

-- allApp funcInAConext typeInAContext = (>>=) typeInAContext customLambda
    -- where customLambda func = ( \x -> return funcInAConext x)



{-
Implement a bind function which is the same as (>>=) for Maybe:

bind :: Maybe a -> (a -> Maybe b) -> Maybe b

-}

bind :: (Maybe a) -> (a -> Maybe b) -> Maybe b
bind value fun = (>>=) value fun















----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-- 29.1
To prove that Applicative is strictly more powerful than Functor, 
write a universal version of fmap, called allFmap, 
that defines fmap for all members of the Applicative type class.

 Because it works for all instances of Applicative, 
the only functions you can use are the methods required by the Applicative type class. 
 
To get you started, here’s your type signature:
allFmap :: Applicative f => (a -> b) -> f a -> f b

-
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  --
--}

allFmap2 :: Applicative f => (a -> b) -> f a -> f b 
allFmap2 normalFunc typeInAContext = pure ( normalFunc ) <*> typeInAContext

