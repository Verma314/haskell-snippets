
Example: 

In the context of a job application, we have a type class ```Candidate```

```
data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show
```

Where ```Grade``` and ```Degree``` are defined as,
```
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
```

We can create a random candidate like so,
```
adityaVerma = Candidate { candidateId = 1, codeReview = A, cultureFit = B, education = BA }
```

To check if the candidate is good we define the function,
```
viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding
                 ,passedCultureFit
                 ,educationMin]   
```

This method above takes a candidate and returns a bool. 

What if we need to operate on a candidate that is in an IO Context, or Maybe (after having been retrieved from a DB), or a whole _list_ of candidates.


**A truly generic function to access candidate, one which can take any monad**
```
assess :: (Monad m) => m Candidate -> m String 
assess candidateInContext = do
                            candidate <- candidateInContext
                           	let isPassed = if (viable candidate) then "passed"
                                           else "fail"
                            return isPassed
```

- This method takes in a ```candidateInContext``` 
- After ```do```, using ```<-```, we extract the individual element from the context
- In the ```let``` we operate on the individual element, transforming it into a "passed" or a "failed"
- return helps us wrap "Passed" or "failed" back to the context


To test this method out, in Maybe context
```
> assess (Just adityaVerma )
Just "fail"
```

In a list
```
> assessCandidateX [adityaVerma , candidate1, candidate2]
["fail","fail","pass"]
```

In IO, we need a helper function to read the element from IO 
```
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
```
The method above takes values from the CLI and returns an ```IO Candidate```.
To test the method out.
```
> assess readCandidate
enter id:
1
enter code grade:
A
enter culture fit grade:
A
enter education:
MS
"passed"
```




