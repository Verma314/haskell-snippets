module Main where    
import qualified Palindrome
              
isPalindrome :: String -> Bool                            
isPalindrome text = text == reverse text


main :: IO ()                                              
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response = if Palindrome.isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response


-- if ghci can't find your module: https://stackoverflow.com/questions/26419321/ghci-cannot-find-modules-of-my-program