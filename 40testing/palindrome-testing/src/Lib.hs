module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char(isPunctuation)

preprocess :: String -> String
preprocess text = filter (not . isPunctuation) text

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preprocess text