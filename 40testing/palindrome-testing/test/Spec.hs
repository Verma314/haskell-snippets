import Lib
import Test.QuickCheck
import Data.Char(isPunctuation)
-- simple implementation of assert, that will be used to write test cases (in the main)
assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement



-- property to be upheld by preprocess
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = filter (not . isPunctuation) text
{-
"Calling preprocess on text should give the same answer as calling
           preprocess on the same text with no punctuation"
-}

prop_punctuationInvariant2 text = isPalindrome text == isPalindrome noPuncText
    where noPuncText = filter (not . isPunctuation) text


-- another property that must be upheld by isPallindrome
prop_reverseInvariant text = isPalindrome text == (isPalindrome (reverse text))


main :: IO ()
main = do 
    --quickCheck prop_punctuationInvariant
    --quickCheck prop_reverseInvariant
    --quickCheckWith stdArgs { maxSuccess = 1000}  prop_reverseInvariant
    quickCheck prop_punctuationInvariant2
    putStrLn "done!"


simpleUnitTests :: IO ()
simpleUnitTests = do
  putStrLn "Running tests..."
  assert (isPalindrome "madam") "passed 'madam'" "FAIL: 'madam'"
  assert (isPalindrome "aditya") "passed 'aditya'" "FAIL: 'raadityacecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  assert ((not . isPalindrome) "random") "passed 'random'" "FAIL: 'random'"
  putStrLn "done!"