module Test where
-- modules


{-
Suppose you need to store the length of an object as a variable. For example:

length :: Int
length = 8

How would you use that value without conflicting 
with the existing length function in Prelude?
-}
length :: String
length = "8"

x = putStrLn (Test.length)


head :: Int
head = 10

doubleLength :: Int 
doubleLength = Test.head * 2

-- if we don't use the "module Test where", 
-- we'd have to use Main.head to distinguish it from the head defined in prelude


{--

in the folder "38modules:

1 the main function gets it's own module Main -- the IO action is handled here
2 Pallindrome logic get it's own module Palindrome.

-}


