import Control.Monad
-- Capstone

-- modelling our tables,
data Name = Name
            { firstName ::String
            , lastName :: String }

instance Show Name where
   show (Name first last) = mconcat [first," ",last]

-- Then each student has a grade level:
data GradeLevel = Freshman
   | Sophmore
   | Junior
   | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student
   { studentId :: Int
   , gradeLevel :: GradeLevel
   , studentName :: Name } deriving Show   

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
           ,(Student 2 Junior (Name "Leslie" "Silko"))
           ,(Student 3 Freshman (Name "Judith" "Butler"))
           ,(Student 4 Senior (Name "Guy" "Debord"))
           ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
           ,(Student 6 Junior (Name "Julia" "Kristeva"))]


-- note how these columns defined in the type are also functions,
-- so to implement a select, we can pass along these "functions" to a list
-- example: studentId (head students)
--          (firstName . studentName) (head students)

-- to implement select:

_select :: ( a -> b ) -> [a] -> [b]
_select func table = do
                    element <- table
                    return (func element)

test01 = _select studentId students
test02 = _select studentName  students
test03 = _select (\x -> (studentName x, id x) ) students

-- although this also works:
test04 = fmap (studentName) students



-- implementing where:
-- 
_where :: (a -> Bool) -> [a] -> [a]
_where _filter table = do 
                       element <- table
                       guard (_filter element)
                       return element 

test05 = _where even 
                (_select studentId students)

-----------------------
-----------------------
-- creating more data before implementing join                 