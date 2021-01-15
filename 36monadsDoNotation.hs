-- do notation

-- revision,
-- to create a helloName IO action 
-- that asks the user for their name and then says hello to them.
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

nameStatementIO :: IO String
nameStatementIO = (>>=) (askForName >> getLine) customLam
    where customLam = (\name -> return (nameStatement name) )


-- the book version
nameStatementIO2 :: IO ()
nameStatementIO2 = askForName >> getLine >>= (\ n -> return (nameStatement n)) >>= putStrLn


{-
Write a program by using the tools of the Monad type class 
that takes a pair of values in a context, 

and then returns the maximum of each pair. Here’s your type signature to get you started:

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
-}

maxTup x = max (fst x) (snd x)
constructTup v1 v2 = (v1, v2)

tupIO :: (Int, Int) -> IO (Int,Int)
tupIO val = pure val

tupIO2 :: IO (Int,Int)
tupIO2 = (fmap constructTup getLineAsInt) <*> getLineAsInt
    where getLineAsInt = read <$> getLine

maxPairM valsInContext = valsInContext >>= (\ x -> return (maxTup x) )

maxPairM2 :: IO Int
maxPairM2 = tupIO2  >>= (\ x -> return (maxTup x) )

maxPairM2' :: IO ()
maxPairM2' = (fmap show maxPairM2) >>= putStrLn

-- random experimentation over
------------------------------------------------------
---------------------- DO ----------------------------


-- We will rewrite the above via DO notation 

-- nameStatementIO2 :: IO ()
-- nameStatementIO2 = askForName >> getLine >>= (\ n -> return (nameStatement n)) >>= putStrLn

nameStatementIO23 :: IO ()
nameStatementIO23 = do 
                    askForName
                    x <- getLine
                    putStrLn (nameStatement x)
                    
