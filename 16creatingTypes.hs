

-- type synonyms

-- String is a type synonym for [Char]


-- let's say we have a function to generate a medical report
paitientInfo :: String -> Int -> Int -> String
paitientInfo name age height = name ++ " " ++ show (age) ++ " years " ++ show(height) ++ " cms."


-- since the codebase might get literred with String, Ints etc, we can use type synonym for the benefit of the devs

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

personInfo :: FirstName -> LastName -> Age -> Height -> String
personInfo firstName secondNamne age height = firstName ++ " "  ++ secondNamne ++ " " ++ 
                                                show (age) ++ " years " ++ show(height) ++ " cms."



-- you can use type synonyms for not just one-on-one name changing,
-- PersonName is a new type as a tuple for FirstName and SecondName
type PersonName = (FirstName, LastName)

getFirstName :: PersonName -> FirstName
getFirstName personName = fst personName


---

{-
Rewrite patientInfo to use your patientName type, 
reducing the total arguments needed to three instead of four.
-}
personInfoAlternate :: PersonName -> Age -> Height -> String
personInfoAlternate person age height = personInfo (getFirstName person) (snd person) age height


------------------------------------------------------------------
------------------------------------------------------------------
------ --- --- --- --- Creating new types  --- --- --- --- --- --- 