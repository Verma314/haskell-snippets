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


----------------------------------------------------------------------
----------------------------XOR---------------------------------------

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = if ( value1 == value2 ) then False
                        else True

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2      


xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair ( zip list1 list2)


----------------------------------------------------------------------
--------- Representing values as bits --------------------------------

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 1 = [True]
intToBits' 0 = [False]
intToBits' givenInt = value : intToBits' divident
                where divident = div givenInt 2 
                      remainder = mod givenInt 2 
                      value = if ( remainder == 1 ) then True
                              else False  

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits givenInt = take paddingNumber (cycle [False]) ++ reversedBits 
                     where reversedBits = reverse (intToBits' givenInt)
                           paddingNumber = maxBits - (length reversedBits)
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)


bitsToChar :: Bits -> Char
bitsToChar [] = toEnum (0)
bitsToChar bitList =  toEnum(valueToAdd + fromEnum(bitsToChar(xs)))
                      where (x:xs) =  bitList
                            valueToAdd = if x then 2 ^ ( length bitList - 1) 
                                         else 0    

---
-- to encrypt:

messageToBits message  = map charToBits message
bitsToMessage bitListofList = map bitsToChar bitListofList

-- test
test1 = bitsToMessage ((messageToBits "a"))
test2 = bitsToMessage ((messageToBits "aditya"))

xorTwoMessages :: String -> String -> [Bits]
xorTwoMessages message1 message2 = xorTwoBitLists (messageToBits message1) (messageToBits message2)

-- type Bits = [Bool]
xorTwoBitLists :: [Bits] -> [Bits] -> [Bits]
xorTwoBitLists bits1 bits2 =  map xoringFunction (zip bits1 bits2)
                              where xoringFunction = (\ toXorTup -> xor (fst toXorTup) (snd toXorTup) )

-- works!

{- testing:

*Main> bitsToMessage (xorTwoMessages "abc" "fsa")
"\a\DC1\STX"


*Main> bitsToMessage (xorTwoMessages "abc" "\a\DC1\STX")
"fsa"

*Main> bitsToMessage (xorTwoMessages "ADI" "ADI")
"\NUL\NUL\NUL"

-}
    

-- todo: One time pad