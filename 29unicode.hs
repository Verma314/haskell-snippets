{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup
import qualified Data.Text.IO as TIO

dharma :: T.Text
dharma = "©©©©©©©©©©"

bgText :: T.Text
bgText = "©©©©©©©©©©ˍˍˍˍˍ˝ƒƒƒƒƒˇˇßœ˚˝ð˝˙œð˘ʼ£¶™•₹ˆ™̐£°"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces          
  where pieces = T.splitOn query fullText                            
        highlighted = mconcat ["{",query,"}"]  

main :: IO ()
main = do 
        let result = highlight dharma bgText
        TIO.putStrLn result  




helloPerson ::  T.Text -> T.Text 
helloPerson name = mconcat[ "Hello" ," " , name , "!"]

main2 :: IO ()
main2 = do
   putStrLn "Hello! What's your name?"
   name <- getLine
   let statement = helloPerson (T.pack name)
   TIO.putStrLn statement


{-Use Data.Text.Lazy and Data.Text.Lazy.IO 
 to rewrite the lazy I/O section from lesson 22 by using the Text type.-}

-- todo