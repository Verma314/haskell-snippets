import qualified Data.Map as Map  
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




-- example from book:

data RobotPart = RobotPart{ name :: String   , 
                            description :: String   , 
                            cost :: Double   , 
                            count :: Int   } deriving Show


leftArm :: RobotPart
leftArm  = RobotPart
   { name = "left arm"
   , description = "left arm for face punching!"
   , cost = 1000.00
   , count = 3
   }

rightArm :: RobotPart
rightArm  = RobotPart
   { name = "right arm"
   , description = "right arm for kind hand gestures"
   , cost = 1025.00
   , count = 5
   }

robotHead :: RobotPart
robotHead  = RobotPart
   { name = "robot head"
   , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
   }


-- we gotta rended the info contained in the RobotPart as HTML,
type Html = String
renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName, "</h2>"
                          ,"<p><h3>desc</h3>",partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount,"</p>"]
 where partName = name part
       partDesc = description part
       partCost = show (cost part)
       partCount = show (count part)


partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where keys = [1,2,3]
       vals = [leftArm,rightArm,robotHead]
       keyVals = zip keys vals



-- given that we have a map 
-- index all the parts we have 

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- Here’s how you can apply renderHtml to a list of values by using <$>.

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts


-- how do we use fmap to create a Map mapping the ints to Html, instead of RobotPart
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = fmap renderHtml partsDB 

-- Now see, how we were able to transform 
-- a whole list of RobotParts into HTML values
-- a whole MAP (int, RobotParts) into Map (int, HTMLRoboParts)

-- all we had was just a simple logic to convert RobotParts to HTMLRoboParts
-- but now we can convert any parameterized type into the type parameterized by another type

-- Functors can be thought of as "mappable" types (really?)



--- IO:

leftArmIO :: IO RobotPart
leftArmIO = return leftArm


-- to IO an HTML, do not write a new logic
leftArmHTML :: IO Html
leftArmHTML = renderHtml <$> leftArmIO


--

-- exercises,

data Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box a) = Box (func a)


-- Box is a functor now
-- now lets write a generic logic:

listifyNumber :: Int -> [Int]
listifyNumber a = Prelude.take 10 (cycle [a] )

morePresents :: Box Int -> Box [Int]
morePresents box = fmap listifyNumber box

--

myBox :: Box Int
myBox = Box 1


wrap :: Int -> Box Int
wrap a = Box a 
--wrap :: Num a => Box a -> Box (Box a)
--wrap box = Box box

-- we gotta make that above wrap logic work with functors
rewrapBox = fmap wrap myBox

-- lets make a simple unwrap
unwrap :: Num a => Box a -> a
unwrap (Box a) = a

unwrapABox = unwrap <$> rewrapBox


{-Write a command-line interface for partsDB that lets the user look up 
  the cost of an item, given an ID. 
  
  Use the Maybe type to handle the case of the user entering missing input.-}

partLookup partId = Map.lookup partId partsDB

-- let us IO the partID
partID :: Int -> IO Int
partID id = return id

-- now lets apply the functor on this partId
mylookup :: Int -> (Map.Map Int RobotPart) -> IO (Maybe RobotPart)
mylookup id partsDB = (\ x -> Map.lookup x partsDB) <$> (partID id)


---------------------------------------------------------------------