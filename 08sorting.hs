import Data.List

author = ("Aditya","Verma")

testFxn = do
        print ( fst author )
        print ( snd author)

testFxn2 = sort [10,9,8,8,7,6]
testFxn3 = sort ["Aditya", "Verma", "Zzzz", "Aaaaa"]



compareByFxn elem1 elem2 = if elem1snd > elem2snd
                            then GT
                            else if elem1snd < elem2snd
                            then LT
                            else if elem1fst > elem2fst
                                then GT
                                else if elem1fst < elem2fst
                                    then LT
                                    else
                                        EQ

                        where elem1snd = snd elem1
                              elem2snd = snd elem2
                              elem1fst = fst elem1
                              elem2fst = fst elem2

testFxn4 =   do
            let mylist = [ ("babc","babc"), ("abcd","abcd"), ("d","d")]
            sortBy compareByFxn  mylist               


testFxn5 givenList = sortBy compareByFxn givenList    




-- rewrite compareLastNames using compare

{--
compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else EQ
                        where lastName1 = snd name1
                             lastName2 = snd name2
--}

compareLastNames name1 name2 = compare lastName1 lastName2
                                where lastName1 = snd name1
                                      lastName2 = snd name2
 



-- get address function 

newYorkFunction name = name ++ "ny " ++ " - " ++ "New York 13134"
sfFunction name = name ++ "sf " ++ " - " ++ " San Fran 1342432"
delhiFunction name = name ++ "del " ++ " - " ++ " Delhi 1342432"

                

getCityFxn location = case location of 
                        "delhi" -> delhiFunction
                        "sf" -> sfFunction
                        "ny" -> newYorkFunction
                        _ -> (\x -> x ++ "  generic city -- wildcard")
                    


abc = getCityFxn "sf"


genFunction cityName = getCityFxn cityName

genAddress cityName name = (genFunction cityName) name


genAddress2 cityName name = (getCityFxn cityName) name

genAddress3 cityName name = locationFunction name
                        where locationFunction = (getCityFxn cityName)

genAddress4 city name = locationFunction name
                where locationFunction = getCityFxn city


{-
getCityFxn city = case city of 
    "delhi" -> delhiFunction
    "sf" -> sfFunction
    "ny" -> newYorkFunction
    _  -> (\name -> (fst name) ++ " " ++ (snd name) ++ " Unknown City" )

generateAddress name city = locationFunction name
                where locationFunction = getCityFxn city

-}
