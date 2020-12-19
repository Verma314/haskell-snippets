

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

-- type constructor: Sex
data Sex = Male | Female
-- the data keyword tells haskell that we are creating a new type
-- the Sex type can be either an instance of Male or Female 
-- Male and Female are data constructors

-- bool is defined like:
-- data Bool = True | False

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

------------------------------------------------------------------
-- Modelling blood group -----------------------------------------

data RhType = Pos | Neg
data ABOType = A | B | AB | O



data BloodType = BloodType ABOType RhType 
-- type^constructor   ^ data constructor

-- a BloodType is an RhType and ABOType

paitient1BT :: BloodType ; 
paitient1BT = BloodType A Pos

paitient2BT :: BloodType ; 
paitient2BT = BloodType AB Neg

paitient3BT :: BloodType ; 
paitient3BT = BloodType AB Pos

-- lets try to print this type out
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO value = case value of 
                    AB -> "AB"
                    A -> "A"
                    B -> "B"
                    O -> "O"

showBloodType :: BloodType -> String
showBloodType (BloodType aboType rhType) = (showABO aboType)  ++ " " ++ (showRh rhType) 
-- ^ pattern matching used


-- which blood group can donate to which other blood group:
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True                      
canDonateTo _ (BloodType AB _) = True                     
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False 


------------------------------------------------------------------
-- Modelling Name type  ------------------------------------------

type MiddleName = String

data Name =  Name FirstName LastName
           | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name firstName lastName) = firstName ++ " " ++ lastName           
showName (NameWithMiddle firstName middleName lastName) = firstName ++ " " ++ middleName ++ " " ++ lastName


myname :: Name ; myname = Name "Aditya" "Verma"


name2 :: Name ; name2 = NameWithMiddle  "Aditya" "Singh" "Verma"


------------------------------------------------------------------
-- Example: Modelling a paitient ---------------------------------

data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient; 
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- to create a Patient using the above method is certainly quite tedious
-- and would get more tedious if we use 

-- Alternatively,
-- we can create a new data type using the Record syntax

data PaitientV2 = PaitientV2 { name :: Name, 
                                sex :: Sex,
                                age :: Int,
                                height :: Int,
                                weight :: Int,
                                bloodType :: BloodType}

-- now creating PaitientV2s is super easy as we no longer need to set the fields by order,
-- we can do it by name

jackie :: PaitientV2
jackie = PaitientV2 { name = Name "Jackie" "Smith",
                      sex = Female,
                      age = 42,
                      weight = 60,
                      height = 170,
                      bloodType = BloodType AB Pos}

-- getters for each field get created automatically
test0 = age jackie
test1 = showBloodType (bloodType jackie )
test2 = showName (name jackie )

