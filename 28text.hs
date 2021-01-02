{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

--T.pack :: String -> T.Text
--T.unpack :: T.Text -> String


key = "MysTRING"
keyText = T.pack key

aWord :: T.Text
aWord = "Cheese"

main :: IO ()
main = do
  print aWord

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- to join
joined = T.unlines ["1234","32435","sdg"]

--------------------

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"


split = T.splitOn breakText exampleText


-- join using a particular delimiter
-- interlace
joinedUsingCustomDelimiter = T.intercalate "---" ["123","456","789"]
-- joinedUsingCustomDelimiter 
-- "123---456---789"


combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]


{-Create your own version of T.lines and T.unlines by using splitOn and T.intercalate.-}

-- T.lines:  
myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n" 

-- T.unlines
myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n" 

----------------------------------------------
----------------- Unicode --------------------

