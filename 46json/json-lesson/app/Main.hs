module Main where
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics



data Book = Book
            { title :: T.Text
            , author :: T.Text
            , year :: Int
            } deriving (Show,Generic)


instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {author="Will Kurt"
              ,title="Learn Haskell"
              ,year=2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook


rawJSON :: BC.ByteString
rawJSON = "{\"author\": \"Will Kurt\",\"title\": \"Learn Haskell\",\"year\": 2017 }"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

-------------
-- Exercises,

--- make Name an instance of ToJSON without Generic:

data Name = Name
    { firstName :: T.Text
    , lastName :: T.Text
    } deriving (Show)


instance ToJSON Name where 
    toJSON (Name firstName lastName ) = object [  "firstName" .= firstName, 
                                                "lastName" .= lastName]



-- Make the Name type into an instance of FromJSON without Generic:
instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "firstName"
                                <*> v .: "lastName"

adi = Name "Aditya" "Verma"
adiJSON = encode adi

--adiInAConextNow = decode ( encode adi)

sampleAdi :: BC.ByteString
sampleAdi =  "{\"lastName\":\"Verma\",\"firstName\":\"Aditya\"}"

adiInAConextNow :: Maybe Name
adiInAConextNow = decode ( sampleAdi)

--
data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , errorCode :: Int
                   } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"


-- more exercises,
{-
Make a Sum type called IntList and use DerivingGeneric to make it an instance of ToJSON. Don’t use the existing List type, but rather write it from scratch. Here’s an example of an IntList:

intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList
-}

data IntList = Cons Int IntList | EmptyList deriving (Show,Eq)

x = Cons 1 $ Cons 2 $ Cons 3 $ EmptyList 

-- making it an instance of ToJson
instance ToJSON IntList where 
    --toJSON EmptyList = object [ "Cons" .= 0, ]
    toJSON (Cons element restOfTheList ) = if (restOfTheList == EmptyList ) 
                                           then  
                                           object [ "Element: " .= element ] 
                                           else object [ "Element: " .= element, " $ " .= toJSON restOfTheList]

jsonedList = encode x
   
-- returns   
{-
{
    "Element:": 1,
    " $ ": {
        "Element: ": 2,
        " $ ": {
            "Element: ": 3
        }
    }
}
-}

-- or one could simply derive Generic :p








main :: IO ()
main = print "hi"


