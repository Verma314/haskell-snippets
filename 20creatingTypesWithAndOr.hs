-- product types:

data AuthorName = AuthorName String String deriving (Show)
data Book =  Book String AuthorName deriving (Show)


authorName :: AuthorName
authorName = AuthorName "Aditya" "Verma"

book :: Book; 
book = Book "MysTRING" authorName


-- using Record syntax:
data BookV2 = BookV2 {
                name :: AuthorName,
                isbn :: String,
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

