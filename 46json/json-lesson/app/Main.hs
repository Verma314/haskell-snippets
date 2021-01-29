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














main :: IO ()
main = print "hi"


