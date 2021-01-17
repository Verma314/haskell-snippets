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

-- create a typeclass Teacher,
data Teacher = Teacher
 { teacherId :: Int
 , teacherName :: Name } deriving Show

-- create a "table" for infomration on teachers
teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
           ,Teacher 200 (Name "Susan" "Sontag")]



-- create another type called Course, which contains info about what teacher teaches which course
data Course = Course
 { courseId :: Int
 , courseTitle :: String
 , teacher :: Int } deriving Show

-- let us create a "table" for courses

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]

-- french has teacher value 100, i.e Simon de Beauvior teaches frecnh 

{- for _join, we’ll be checking to see whether a given property of 
    data in one list is equal to another property in another list.

the type signature will be:

_join :: [a] -> [b] -> (a->c) -> (b -> c) -> [(a,b)]

-}

cartesianProd a b = [ (ele1,ele2) |  ele1 <- a, ele2 <-b]

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join list1 list2 property1 property2 = do
                                        cartElement <- cartesianProd list1 list2
                                        let firstElement = fst cartElement
                                        let secondElement = snd cartElement
                                        guard( property1 firstElement == property2 secondElement )
                                        return cartElement


-- see how you don't need an extra cartesianProd function, you can do that in your do block itself.
-- example,
_join2 list1 list2 property1 property2 = do
                                         ele1 <- list1
                                         ele2 <- list2
                                         guard ( property1 ele1 == property2 ele2)
                                         return (ele1,ele2,"end")



-- todo: how to combine these  _select, _join, _where functions