-- functors

{-Q1:

Write the function 
reverseMaybe :: Maybe String -> Maybe String 
that reverses a Maybe String and returns it as a Maybe String.

-}
reverseMaybe :: Maybe String -> Maybe String 
reverseMaybe (Just str) = (Just (reverse str))
reverseMaybe Nothing = Nothing


