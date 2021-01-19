import Control.Monad
import Control.Applicative -- for the guard function, and the Alternative
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

-- find (teachers,courses) where teacherId == teacher
joinData = (_join teachers courses teacherId teacher)    

-- let us get some data from the above result now:
whereResult = _where ((== "English") . courseTitle . snd) joinData
-- snd gets the "Course" from the (teachers,courses) result,
-- courseTtitle is getting the couseTitle, == English is filtering for 
-- courses which are engligh
-- so we end up getting all the (Teacher,Course) pairs where the course is english


-- now we are getting a whole list of (---, ----)  tups as result, 
-- how to "_select" the right values,
selectResult = _select (teacherName . fst) whereResult
-- selecting the teacherName from the fst 



---------------------------------
---------------------------------
-- How to restructure the above staments to make it look more like an sql?
-- we use lambdas.

-- how:

-- from the book's author (W. Kurt)
-- we create a common interface: 
--  "_hinq" that will take the _select,_join, and _where queries 
-- in the order you expect and then use lambdas behind the scenes 
-- to restructure everything.

_hinq selectQuery joinQuery whereQuery = (\ joinData -> 
                                                    (\whereResult -> selectQuery whereResult) (whereQuery joinData)
                                            ) joinQuery
-- kinda tedious ^
-- let us re-write it,
-- join "creates" the initial list of tuples, so we create that first,
-- also, join data does not depend on other queries, so joinData == joinQuery

-- next, where "acts" on the result from join
-- at last, the "select" query acts on the "whereData"
_hinq2 selectQuery joinQuery whereQuery = selectData
    where joinData = joinQuery
          whereData = whereQuery joinData
          selectData = selectQuery whereData
-- much mroe beautiful and readable, I think!


-- Example to test this out:
finalResult :: [Name]
finalResult = _hinq2 (_select (teacherName . fst))                    
                    (_join teachers courses teacherId teacher)                    
                    (_where ((== "English") .courseTitle . snd))

-- not filtering anything:, put a generic conditional, which will return true for everything
finalResult2 :: [Name]
finalResult2 = _hinq2 (_select (teacherName . fst))                    
                    (_join teachers courses teacherId teacher)                    
                    (_where ((\ _ -> True ) .courseTitle . snd))


-- can we re-design _hinq so that we can omit where statment?
-- the book says, "You’ll use a HINQ type that will have two constructors."


-- we will need to re-write _select, _where and _join to work with monads
-- although we use the guard() which works on the Alternative type class
-- hence we need to re-write the type signature to indicate that.
-- (currently we have written these functions for lists, the guard function 
-- works, because List and Maybe are already types of Alternative)

_select3 :: Monad m => (a -> b) -> m a -> m b
_select3 func table = do
                    element <- table
                    return (func element)

_where3 :: (Monad m, Alternative m) => (a -> Bool) -> m a  -> m a
_where3 _filter table = do 
                       element <- table
                       guard (_filter element)              
                       return element

_join3 :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b) 
_join3 list1 list2 property1 property2 = do
                                         ele1 <- list1
                                         ele2 <- list2
                                         guard ( property1 ele1 == property2 ele2)
                                         return ( ele1,ele2)

{-
----- fyi:
:t _select3 (teacherName . fst)
_select3 (teacherName . fst) :: Monad m => m (Teacher, b) -> m Name

hence the type for the select, it takes a monad, and returns one,


> :t  _join3 teachers courses teacherId teacher
_join3 teachers courses teacherId teacher :: [(Teacher, Course)]

_join is bunch of stuff inside a monad
--------
-}


-- let us create a common interface to beautify the queries ie make them readable
data HINQ m a b =   HINQ (m a -> m b ) (m a) (m a -> m a)
                    | HINQ_ (m a -> m b ) (m a) 


-- why the HINQ m a b  typeclass?
-- what is it? 
{-
It is an 'or' type class, that can either be 
HINQ (m a -> m b ) (m a) (m a -> m a)     or     HINQ_ (m a -> m b ) (m a) 

the kind of  (* -> *) -> * -> * -> *
the first type it takes is a monad (* -> *),
them a ranadom type a, and then another random type b

for its data constructors we have HINQ and HINQ_
that accept three types: 
1. one: the return value of a _select (partial) function.  it takes in a table and returns one ( m a -> m b ). a monad transformation
2. two: a join. just has a bunch of returned data in a monad
3. a where : filters the things inside a monad (m a -> ma) 


We use this type class to be like a container type class for queries:

like this one:
HINQ (_select3 (teacherName . fst))
               (_join3 teachers courses teacherId teacher)
               (_where3 ((== "English") . courseTitle . snd))   

--
having a container is not enough,

we will now, define a new method runHINQ, that takes in these HINQ containers,
and via pattern matching executes them, using the _hinq function/common interface we had defined before.

-}

-- common interface,
runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause
                                                (_where3 (\_ -> True))

-- creating the query via the type class HINQ
query1 :: HINQ [] (Teacher, Course) Name
query1  = HINQ (_select3 (teacherName . fst))
               (_join3 teachers courses teacherId teacher)
               (_where3 ((== "English") . courseTitle . snd))                                                

-- this ^  is just a ```HINQ m a b ``` type, we need to give this query to runHINQ (which can actually exeucte it)
queryExecute = runHINQ query1