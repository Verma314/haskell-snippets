-- monoids are semigroups but with one more constraint
-- i.e monoid requires an identity element for the type.

test0 =  mconcat ["does"," this"," make"," sense?"]


-- Law 1 of monoids:
a = mappend mempty test0
y = a == test0

-- y == True

-- Law 2 of monoids:
{-
Reverse of Law 1:
 
 mappend x mempty is x

-}

-- Law 3 : associativity:
{-
mappend x (mappend y z) == mappend (mappend x y) z.

x ++ ( y ++ z) = (x ++ y ) ++ z
-}

-- Law 4: 

{-
mconcat = foldr mappend mempty

From the book
"Note that the reason mconcat uses foldr instead of foldl is due to the way that foldr can work with infinite lists, whereas foldl will force the evaluation."

-}


-----------------------------------------------------------
----------------Practical example : Monoids----------------

-- lets take the cartesian product of two lists
-- [1,2,3,4,5] x [6,7,8]
-- (1,6),(1,7),(1,8),(2,6),(2,7),(2,8)....

mapper list element = map (\ x -> (element,x)) list

cartesianProduct :: [a] -> [a] -> [[(a,a)]]
cartesianProduct list1 list2 = map (mapper list2) list1

cartesianProductLevelled :: [a] -> [a] -> [(a,a)]
cartesianProductLevelled list1 list2 = foldl (++) []  (cartesianProduct list1 list2 )

cartCombine :: (a -> a -> a) -> [a] -> [a] -> [a]
cartCombine func list1 list2 = map (\(x,y) -> func x y) (cartesianProductLevelled list1 list2)



-- Example from 'Get Programming in Haskell' by W. Kurt --- 
-----------------------------------------------------------
------------- Building porbability table ------------------

type Events = [String]
type Probs = [Double]

-- a probability table is a combination of the above two
data PTable = PTable Events Probs  --deriving (Show) ; we will implement Show ourselves

-- create a probability table
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedEvents
                            where normalizedEvents = map (\x -> x / probabilitySum) probs
                                  probabilitySum = sum probs

showPair :: String -> Double -> String
showPair event probability = mconcat [event," | ", show probability,"\n"]

instance Show PTable where 
    show (PTable events probs) = mconcat pairs 
                                where pairs = zipWith (showPair) events probs
                                


combineEvents events1 events2 = cartCombine eventCombiner events1 events2
                                where eventCombiner = (\x y -> mconcat [x,"-",y])

combineProbs probs1 probs2 = cartCombine (*) probs1 probs2
                                            
-- let us make PTable an instance of Semigroup
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable combinedEvents combinedProbs
                           where combinedEvents = combineEvents e1 e2
                                 combinedProbs = combineProbs p1 p2 

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]      


-- let us make the PTable a monoid as well:
instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

-- these are all semigroup objects, combining them will generate the resultant probability
test1 = foldr (<>) (PTable [] []) [coin,coin,spinner]

-- or:
test2 = mconcat [coin,coin]



---------------------------
-- q 17.2 
-- reimplementing with Events and Probs as monoids


data EventX = EventX String deriving (Show)
data ProbX = ProbX Double deriving (Show)

--- begin:
--- for testing
e1 = EventX "head"
e2 = EventX "tail"

p1 = ProbX 0.5
p2 = ProbX 0.5
--- end;

instance Semigroup EventX where 
    (<>) (EventX e1) (EventX e2) = EventX combinedEvent
                                   where combinedEvent  = e1 ++ "-" ++ e2

instance Monoid EventX where
    mempty = EventX ""

instance Semigroup ProbX where
    (<>) (ProbX p1) (ProbX p2) = ProbX (p1*p2)

instance Monoid ProbX where
    mempty = ProbX 0.0


data ProbTable = ProbTable [EventX] [ProbX]


-- create a ProbTable:
-- begin:
pt = ProbTable [e1,e2] [p1,p2]
--
combEvents :: [EventX] -> [EventX] -> [EventX]
combEvents e1 e2 = cartCombine (<>) e1 e2

combProbs :: [ProbX] -> [ProbX] -> [ProbX]
combProbs p1 p2 = cartCombine (<>) p1 p2 


coinX = ProbTable [EventX "head",EventX "tail"] [ProbX 0.5, ProbX 0.5]

instance Semigroup ProbTable where
    (<>) (ProbTable e1 p1) (ProbTable e2 p2) = ProbTable (combEvents e1 e2) (combProbs p1 p2)


showProbPair :: EventX -> ProbX -> String
showProbPair event prob = mconcat [show event , " | " , show prob  , "\n"]

instance Show ProbTable where 
    show (ProbTable e1 p1) = mconcat (zipWith (showProbPair) e1 p1    )

-- testing:
combinedProbTable = pt <> pt <> pt

pt2 = ProbTable [EventX "A1",EventX "A2",EventX "A3"] [ProbX 0.35,ProbX 0.35,ProbX 0.30]

