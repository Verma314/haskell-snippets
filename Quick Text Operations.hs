{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
-- split sttring 
sampleInput :: T.Text ; 
sampleInput = "this,is,input"
splits = T.splitOn "," sampleInput 

{-
*Main T> splits
["this","is","input"]
-}



-- join a list/text
-- to join
joined = T.unlines ["1234","32435","sdg"]




-- join using a particular delimiter
-- interlace
joinedUsingCustomDelimiter = T.intercalate "---" ["123","456","789"]
-- joinedUsingCustomDelimiter 
-- "123---456---789"



