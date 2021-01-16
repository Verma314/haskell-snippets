import qualified Data.Map as Map
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

nameStatementIO3 :: IO ()
nameStatementIO3 = do 
                    askForName
                    x <- getLine
                    putStrLn (nameStatement x)
                    
nameStatementIO4 :: IO ()
nameStatementIO4  = askForName >> 
                    getLine >>= 
                    ( \ n -> return (nameStatement n)) >>=
                    putStrLn
                    
                    
-----------------------
-----------------------

experiment y = let x = 4
                in x + y

experiment2 y = (\ x -> x + y) 4


----------------------

-- Rewrite echo by using do-notation.

echo :: IO ()
echo = do
    x <- getLine
    putStrLn (x)



-----------------------------------
------ code reuse using monads  ---

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding
                 ,passedCultureFit
                 ,educationMin]   


adityaVerma = Candidate { candidateId = 1, codeReview = A, cultureFit = B, education = BA }

isAdityaVermaViable = viable adityaVerma


readInt :: IO Int
readInt = getLine >>= (return . read)
-- read just converts, say, String -> Int,  return puts the wrapper on it.

-- or
readInt3 :: IO Int
readInt3 = read <$> getLine
-- how are these different? esp the first two

readGrade :: IO Grade
readGrade = getLine >>= (return . read)
-- revision: getLine returns an IO String this gets piped >>= to (return . read ) which takes String -> IO String


readDegree :: IO Degree
readDegree = getLine >>= (return . read)

{-With these helper actions, you can create a single IO action that reads in a candidate.-}

readCandidate2 :: IO ()
readCandidate2 = do 
                putStrLn "Enter Grade for code review"
                grade <- readGrade
                putStrLn "Enter Grade for culture fit"
                cultureFit <- readGrade
                putStrLn "Enter Education BA/ MS / Phd"
                edu <- readDegree
                putStrLn "Enter id"
                id <- readInt
                let candidate = Candidate { candidateId = id, codeReview = grade, cultureFit = cultureFit, education = edu}
                let viability = viable candidate
                putStrLn ( show viability)
-- the above is bad because it is not modularized,



-- to make it more moduralized, split responsibility
readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })


assessCandidate :: IO String
assessCandidate = do
                    candidate <- readCandidate
                    let passed = viable candidate
                    if passed then return "passed"
                    else return "failed"


-- Rewrite readGrade with do-notation.                     
readGrade2 :: IO Grade
readGrade2 = do 
            grade <- getLine
            return (read grade)


-- Revision: without do notation:
readGrade3 :: IO Grade
readGrade3 = putStrLn "enter grade " >> getLine >>= ( return . read )


--- talking about code reuse, let us do some computations,
-- if the candidate is wrapped in a Maybe


-- Step 1 Creating a Data.Map

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }


candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList ( zip [1,2,3] [candidate1,candidate2,candidate3] )

assessCandidateMaybe2 :: Int -> Maybe String
assessCandidateMaybe id = do
    candidate <- Map.lookup id candidateDB -- <- is used because we wanna use it "normally"
    let assessment = viable candidate
    let statement = if (assessment) then "passed"
                    else "failed"
    return statement


assessCandidateMaybe2 :: Int -> Maybe String
assessCandidateMaybe2 id =
    (Map.lookup id candidateDB) >>= 
    ( return . viable ) >>= 
    ( \ x -> if (x) then return  "passed" else return "failed")
    



-- todo: processing candidates in the context of a list