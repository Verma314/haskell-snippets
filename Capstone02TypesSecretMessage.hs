rotN :: (Enum a, Bounded a) => Int -> a -> a
rotN alphabetSize element = toEnum ( rotation)
                             where rotation = mod resultant alphabetSize 
                                   newElementInIntForm = fromEnum element
                                   resultant = newElementInIntForm + (div alphabetSize 2 )
                                   


-- create a custom enum
data SixLetterAlphabet = A0 | A1 | A2 | A3 | A4 | A5 | A6 deriving (Show,Eq,Enum,Bounded)