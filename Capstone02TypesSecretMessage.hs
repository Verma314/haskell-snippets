rotN :: (Enum a, Bounded a) => Int -> a -> a
rotN alphabetSize element = toEnum ( rotation)
                            where rotation = mod resultant alphabetSize 
                                  newElementInIntForm = fromEnum element
                                  resultant = newElementInIntForm + (div alphabetSize 2 )
                            
                        
                                   


-- create a custom enum
data SixLetterAlphabet = A0 | A1 | A2 | A3 | A4 | A5 | A6 deriving (Show,Eq,Enum,Bounded)

data FiveLetterAlphabet = B1 | B2 | B3 | B4 | B5 deriving (Show, Eq, Enum, Bounded)

{-
rotNV2 :: (Enum a, Bounded a) => Int -> a -> a
rotNV2 alphabetSize element = toEnum ( rotation)
                            where rotation = mod resultant alphabetSize 
                                  newElementInIntForm = fromEnum element
                                  resultant = if (mod  alphabetSize 2 == 0 ) 
                                                 then newElementInIntForm + (div alphabetSize 2 )
                                                 else 1 + newElementInIntForm + (div alphabetSize 2 )

-}
-- Alternate way to implement:
rotNV2 :: (Enum a, Bounded a) => Int -> a -> a
rotNV2 alphabetSize element = toEnum ( rotation)
                            where rotation = mod resultant alphabetSize 
                                  newElementInIntForm = fromEnum element
                                  resultant = if (mod alphabetSize 2 == 0 ) 
                                              then moveBy
                                              else 1 + moveBy
                                              where moveBy = newElementInIntForm + (div alphabetSize 2)

-- idk how to make the rotN cipher symmetric for ODD alphabet size symmetric
-- let us implement a way to encrypt entire string

rotCharacters element = rotNV2 alphabetSize element
                        where alphabetSize = 1 + fromEnum (maxBound :: Char)


-- alphabetSize = 26
rottN :: (Enum a, Bounded a) => Int -> a -> a
rottN alphabetSize element =   toEnum (adjustedRotation )
                              where elementToInt = fromEnum (element)
                                    rotation = elementToInt + (div alphabetSize 2)
                                    alphabetRangeRotation = rotation `mod` (fromEnum 'z')
                                    adjustedRotation = if alphabetRangeRotation < (fromEnum 'a')
                                                       then (fromEnum 'a') + alphabetRangeRotation - 1 
                                                       else alphabetRangeRotation

-- Type signature on a partial application! Yays
rot13x :: (Enum a,Bounded a) => a -> a
rot13x = rottN 26                                             

-- now lets apply this crypto algo on a whole string, using map ofc
encryptString :: String -> String
encryptString toEncrypt = map rot13x toEncrypt
-- this only works with lowercase :p