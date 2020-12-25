-- product types:

data AuthorName = AuthorName String String deriving (Show)
data BookV1 =  BookV1 String AuthorName deriving (Show)


authorName :: AuthorName
authorName = AuthorName "Aditya" "Verma"

book :: BookV1; 
book = BookV1 "MyString" authorName


-- using Record syntax:
data BookV2 = BookV2 {
                name :: AuthorName,
                bookIsbn :: String,
                title :: String,
                year :: String,
                price :: Double
                    } deriving (Show)

-- testing the automatic getters
testBook = BookV2 authorName "62738U34241" "KEUTR" "DJSDFS" 342.23
testNameGetter = name testBook



data AuthorNameV2 = AuthorNameV2 {
                        firstName :: String,
                        lastName :: String
                    }


-----------------------------------------------------------

type FirstName = String
type LastName = String
type MiddleName = String
{-
data Name = Name FirstName LastName 
   | NameWithMiddle FirstName MiddleName LastName deriving (Show)

-- old ^

note below how ORing to Name, doesn't break anything
-}

data Name = Name FirstName LastName 
   | NameWithMiddle FirstName MiddleName LastName 
   | NameWithInitials Char Char LastName deriving (Show)



-- lets create an abstraction for artist and author,
-- so that we can type both of these types together.

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)


-- Author data type depends on just name, straightforward
data Author = Author Name deriving (Show)

-- for Artist, we will need to use 'or' type / 'sum' type, like this:
data Artist = Person Name | Band String deriving (Show)


-- also remember that Artist and Name are types, not type classes.
-- (as a revision: this Artist class can be made a type of certain type class (like Show), by deriving from Show, or implementing Show)

-- the name type is helping both Artist and Author,
-- it can help many more classes.
-- and the best thing is that, you can literally add to Name without breaking anything
-- for 'AND' types, if you add more data members in Name, you would have to add that particular
-- property to all the types that depend on that type. 
-- Adding via 'OR' to Name, wont break anything.


-- lets create an instance:
hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (NameWithInitials 'H' 'P' "Lovecraft"))

-- Name type was able to adjust the change (addition of NameWithInitials) with ease
-- new data constructors can be easily added too, for example (from book):

{-
data Name = Name FirstName LastName
   | NameWithMiddle FirstName MiddleName LastName
   | TwoInitialsWithLast Char Char LastName
   | FirstNameWithTwoInits FirstName Char Char


In, Java, a functionality like this would be very weird:

public class Name {
    String firstName;
    String lastName;
    String middleName;
    char firstInitial;
    char middleInitial;
    char lastInitial;
}

Lots of extra code would be needed in Java to account for the myriad behaviours with name only.

Making sure the state of Name is valid would be tough.
-}





--------------------------------------------------------------------
----- Section 16.3 from Will Kurt's Get programming with Haskell----
----- To implement the store ---------------------------------------

data Book = Book {
     author    :: Creator
   , isbn      :: String
   , bookTitle :: String
   , bookYear  :: Int
   , bookPrice :: Double
   } deriving (Show)

data VinylRecord = VinylRecord {
     artist        :: Creator
   , recordTitle   :: String
   , recordYear    :: Int
   , recordPrice   :: Double
   }   deriving (Show)

-- the record syntax actually creates a bunch of functions in the environment, like so:
-- recordYear (VinylRecord _ _ val _ ) = val
-- hence both these Types have to have distinct price arguments, bookPrice and recordPrice
-- using price in both would create two functions of the same name and cause an error


-- to create a store item:
-- data StoreItem = BookItem Book | RecordItem VinylRecord 
-- old ^

-- if we later add a new type of item in our store:

data CollectibleToy = CollectibleToy {
     toyName :: String
   , descrption :: String
   , toyPrice :: Double
} deriving (Show)

-- now we can update the StoreItem easily:
data StoreItem = BookItem Book | RecordItem VinylRecord  | CollectibleItem CollectibleToy deriving (Show)


-- now lets write a generalized price function, which takes any object of type StoreItem
-- and returns its price

getPriceOfStoreItem :: StoreItem -> Double
getPriceOfStoreItem (BookItem book) = bookPrice book
getPriceOfStoreItem (RecordItem vinyl) = recordPrice vinyl
getPriceOfStoreItem (CollectibleItem item) = toyPrice item

-- again ^ easily extensible, easy to add new items, new logic
-- increases expressiveness





{-
Write a madeBy function
 that has the type StoreItem -> String 
 and does its best to determine who made the StoreItem.
-}

madeBy :: StoreItem -> String
madeBy (BookItem book) = "Book Creator: " ++  show (author book)
madeBy (RecordItem record) = " Record Creator " ++ show ( artist record)
madeBy (CollectibleItem item) = "Ancient collectible item, vintage, no one knows"


myRandomBook = Book { author = hpLovecraft , isbn = "asdadad", bookTitle = "DDSW", bookYear = 1996, bookPrice = 123.44}
storeItemInventoryBook1 = BookItem myRandomBook
testofFunction_madeBy = madeBy storeItemInventoryBook1




--

{-Q16.2

Create a Shape type that includes the following shapes:
 Circle, Square, and Rectangle. 
Then write a function to compute the perimeter of a Shape as well as its area.-}

type Radius = Double
type Side = Double
data Shape = Circle Radius | Square Side | Rectangle Double Double deriving (Show)

area :: Shape -> Double
area (Circle radius) = pi * radius * radius
area (Square side) = side * side
area (Rectangle length width) = length * width


{-
testing:

*Main> myShape = Circle 10
*Main> 
*Main> area myShape
314.1592653589793
-}





