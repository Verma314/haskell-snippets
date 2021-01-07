-- functors

{-Q1:

Write the function 
reverseMaybe :: Maybe String -> Maybe String 
that reverses a Maybe String and returns it as a Maybe String.

-}
reverseMaybe :: Maybe String -> Maybe String 
reverseMaybe (Just str) = (Just (reverse str))
reverseMaybe Nothing = Nothing

-----------------------------------
-----------------------------------



successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing


incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing
-- for a trivial function on a 'type in context' (Maybe Int),
-- we had to do pattern matching,
{-  lets say we writing a whole library, and have to create a lot of functions 
    on `Maybe Int`,

    would we keep doing the pattern matching?
    
    what if a ( \x -> x + 1 ) function already exists?
    we would end up creating two  versions of all functions, 
    (for the "normal" type, and the parameterzied type)
-}


-- lets make Maybe a functor:
{--
instance Functor Maybe where 
    fmap func (Just a) = Just (func a)
    fmap func Nothing = Nothing



if you execute the above, it throws an error,
        Duplicate instance declarations:
      instance Functor Maybe -- Defined at 32functors.hs:43:10
      instance Functor Maybe -- Defined in ‘GHC.Base’


Because Maybe is already a Functor.

--} 

-- *Main> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- so now,

-- int in a list
incList = fmap (+1) [1,2,3]

incValue = fmap (+1) successfulRequest
incValue2 = fmap (+1) (Just 10)

-- fmap is mapping from one value in a conext to another.


-- alternative syntax:
incJust1 = (+1) <$>  (Just 1)

-- works on any function that works on the nested type
convertLookedUpValToString =  fmap show (Just 10)
-- Just "10"

convertLookedUpValToStringAlt = show <$> Just 10

reverseMaybeWithFunctors :: Maybe String -> Maybe String
reverseMaybeWithFunctors maybeStr = reverse <$> maybeStr

-- what is the functor here?
