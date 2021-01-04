{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import qualified Data.Text.IO as TIO
import Control.Monad
{-Q1:

Write a function that takes numbers in ASCII character form 

and converts them to Ints. 

For example, make the following an Int:-

bcInt :: BC.ByteString
bcInt = "6"
-}

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt givenByteString = (read . BC.unpack) givenByteString

intToBC :: Int -> BC.ByteString
intToBC givenInt = BC.pack [toEnum (givenInt `mod` 255)]

{-This function will take the location of the byte to be replaced, 
the Int value of the new Char/Byte to go there, and the bytes of the image file
.. Then you’ll drop one from the rest of the bytes to make room for your new byte. 
Finally, you’ll concatenate the new byte in the middle of these two sections

-}
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,middlePart,modifiedAfter]
                                where (before,after) = BC.splitAt loc bytes
                                      modifiedAfter =  BC.drop 1 after
                                      middlePart = intToBC charVal


{- You’ll be using randomRIO from System.Random. 
randomRIO will take a pair of values in a tuple and randomly give you a number in that range. 
Your IO action will be named randomReplaceByte. 
All randomReplaceByte needs to do is pick two random numbers: one for the Char, and one for the location.-}
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do                                      
                            randomChar <- randomRIO(0,255)
                            randomLocation <- randomRIO(0,BC.length bytes)
                            let modifiedBytes = replaceByte randomLocation randomChar bytes
                            return modifiedBytes


{-Now you can use this IO action in your main to modify your image file:-}

glitch :: FilePath -> IO ()
glitch filename = do
            --putStrLn "Enter filename"
            --filename <- getLine
            contents <- BC.readFile filename
            --putStrLn (BC.unpack contents)
            glitched <- randomReplaceByte contents
            BC.writeFile "out.jpg" glitched

{-
<- is used because getLine returns an IO String , but we want to use name like a regular String

let statement = hello name -- you have to use let whenever we create variables that AREN'T IO type
-}        


retrieveRandomByte :: BC.ByteString -> BC.ByteString
retrieveRandomByte bytes = modifiedAfter
                           where modifiedAfter =  BC.drop ((BC.length bytes) - 1) bytes
                           
                           


computeNumberOfBytes :: IO ()
computeNumberOfBytes = do
            putStrLn "Enter filename"
            filename <- getLine
            contents <- BC.readFile filename
            putStrLn (show  (BC.length contents ) ++ " Bytes" )



generateRandomChar :: IO Char
generateRandomChar = do                                      
                     randomChar <- randomRIO(0,255)
                     return (toEnum randomChar)


{- Here’s your sortSection function, 
which takes a starting point of the section, 
a size of the section, and the byte stream.-}                     

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,sortedSection,ending] 
                                where (before,rest) = BC.splitAt start bytes
                                      (target,ending) = BC.splitAt size rest
                                      sortedSection = BC.sort target
 

randomSortSelection :: BC.ByteString -> IO BC.ByteString
randomSortSelection bytes = do                                      
                            randomStartLocation <- randomRIO(0,BC.length bytes)
                            randomSize <- randomRIO(0,BC.length bytes `div` 2)
                            let modifiedBytes = sortSection randomStartLocation randomSize bytes
                            return modifiedBytes



glitch2 :: String -> IO ()
glitch2 filename = do
            contents <- BC.readFile filename
            glitched <- randomSortSelection contents
            BC.writeFile "out.jpg" glitched


glitch3 :: String -> IO ()
glitch3 filename = do
            contents <- BC.readFile filename
            glitched1 <- randomSortSelection contents
            glitched2 <- randomReplaceByte glitched1
            glitched3 <- randomSortSelection glitched2
            glitched4 <- randomReplaceByte glitched3
            glitched6 <- randomSortSelection glitched4
            BC.writeFile "out.jpg" glitched6          
        
-- how to foldl the glitch 
glitch4 :: String -> IO ()
glitch4 filename = do
                    contents <- BC.readFile filename
                    out <- foldM (\bytes func -> func bytes ) contents [randomReplaceByte, randomSortSelection,randomReplaceByte,randomSortSelection]
                    BC.writeFile "out2.jpg" out

                    