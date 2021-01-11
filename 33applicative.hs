import qualified Data.Map as Map
-- Applicative

-- You want to combine a first- and last-name string to create a person’s name:
--  "Alan" ++ " " ++ "Turing".
-- The trouble is, both your first and last names are Maybe Strings
--  because they come from an unreliable source and might be missing. 

-- How can you combine these Strings and return a Maybe String for the name?

-- one way is: in which we are manually doing a lot of pattern matching.
-- but is there a better way?



type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]


-- how do we compute the distance b/w two cities?
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
 where rlat = toRadians lat
       rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where (rlat1,rlong1) = latLongToRads coords1
       (rlat2,rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1
       a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
       c = 2 * atan2 (sqrt a) (sqrt (1-a))
       earthRadius = 3961.0


-- let us try to get values from locationDb for two cities
-- and take those two values and give it to haversine to compute the distance

getCoordinatesOfCity :: String -> Maybe LatLong
getCoordinatesOfCity city =  Map.lookup city locationDB



{-
unwrap (Just val) = val

compute :: String -> String -> IO String
compute city1 city2 = do
                      let city1Lat = getCoordinatesOfCity city1
                      let city2Lat = getCoordinatesOfCity city2
                      if ( ( city1Lat != Nothing ) || ( city2Lat != Nothing )  )
                        let city1Val = unwrap city1Lat
                        let city2Val = unwrap city2Lat
                        return "ha"
-}


{-. What you want to end up with is an IO action 
that takes a Maybe value for your distance and either prints the distance 
or tells the user that an error occurred.
-} 
printDistance :: Maybe Double -> IO ()
printDistance (Just distance ) = putStrLn (show distance)
printDistance Nothing = error "Not a distance"
                                                        

-- Write addMaybe for adding two Maybe Ints.
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a+b)
addMaybe _ _ = Nothing


--addMaybe Nothing a = a
--addMaybe a Nothing = a



{-
Can we create an Applicative Type?

Okay fmap in functor is defined:
fmap :: Functor f => (a -> b) -> f a -> f b

example:
fmap (add) (Just 10) (Just 20)
...

What we want?
zmap :: Functor f => (a -> c -> d) -> f a -> f c -> f d
fmap :: Functor f => (a -> b     ) -> f a -> f b

-- create a function which 
b = c -> d

and 
f b = f c -> f d

to Concretize:

add = b  

Maybe add = 


nope.
-}                                      


-- lets say we have a partial function,

distanceFromNY = haversine newYork
    where (Just newYork) = getCoordinatesOfCity "New York" 




-- <?> :: (a -> b -> c) -> f a -> f b -> f c

-- zmap :: (a -> (b -> c)) -> f a -> (f b -> f c)
-- 
--zmap normalType partialFunctionNormal typeInTheContext normalTypeInConext = <$>  partialFunctionNormal
-- <$> ::  (b -> c ) -> f b -> f c 


-- use this somehow:
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> f a -> f b

x func (Just v) anotherJust = Just (func v) <*> anotherJust

z = ( fmap(+) (Just 100)) <*> (Just 100)

z2 a b= ( fmap(+) a) <*> b 

zmap func a b = ( fmap func a ) <*> b  -- dang.

-- how to implement Applicative <*> for Maybe type:
mapStar (Just func) (Just a) = Just (func a)


-- to create a generic function which re-uses functions 
-- mapped from regular type and uses them with parameterized types:
regularFunctionInParameterized :: Applicative f => ( a -> b -> c ) -> f a -> f b -> f c  
regularFunctionInParameterized binaryfunc p_obj1 p_obj2 = usingApplicative
    where functionInAContext = fmap binaryfunc p_obj1
          usingApplicative = functionInAContext <*> p_obj2




{-
Use the pattern for using binary values in a context for the functions 

(*), div, and mod on these two values:


-}

val1 = Just 10
val2 = Just 5

soln1 = (fmap (*) (Just 10)) <*> (Just 5)
soln2 = (fmap div val1) <*> val2
soln3 = (mod <$> val1) <*> val2

-- Multi argument function in IO using <*> and <$>
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)


readInt :: IO Int
readInt = read <$> getLine 



minOfThreeIO :: IO Int
minOfThreeIO = (fmap minOfThree readInt) <*> readInt <*> readInt



mainX :: IO ()
mainX = do
        putStrLn "Enter three numbers"
        result <- minOfThreeIO
        putStrLn (show result)
        


-- for haversine we can do 
test1 = Just haversine <*> (getCoordinatesOfCity "New York") <*> (getCoordinatesOfCity "Arkham")
--Just 207.3909006336738

-- or, the more sane, beacuase we might not know the context,
test2 = haversine <$> (getCoordinatesOfCity "New York") <*> (getCoordinatesOfCity "Arkham")
-- Just 207.3909006336738


test3 =  minOfThree <$> (Just 10) <*> (Just 20) <*> (Just 30)
--Just 10


data User = User {name :: String, id :: Int, score :: Int} deriving (Show)

maybeUser maybeName maybeId maybeScore =
    User <$> maybeName <*> maybeId <*> maybeScore