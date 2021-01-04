{-# LANGUAGE OverloadedStrings #-}   
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Author = T.Text
type Title = T.Text


-- using record syntax to create a Book type
data Book = Book { author :: Author, title :: Title} deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat [ "<p>\n", titleInTags, authorInTags, "</p>\n"]
    where titleInTags = mconcat ["<strong>" ,(title book),"</strong>\n"] 
          authorInTags = mconcat ["<em>",(author book),"</em>\n"]


book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race"
   ,author = "Ligotti, Thomas"
   }

book2 :: Book
book2 = Book {
    title = "A Short History of Decay"
   ,author = "Cioran, Emil"
   }

book3 :: Book
book3 = Book {
    title = "The Tears of Eros"
   ,author = "Bataille, Georges"
   }


booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n"
                             , "<head><title>books</title>"
                             ,"<meta charset='utf-8'/>"                
                             ,"</head>\n"
                             , "<body>\n"
                             , booksHtml
                             , "\n</body>\n"
                             , "</html>"]
   where booksHtml = (mconcat . (map bookToHtml)) books


myBooks :: [Book]
myBooks = [book1,book2,book3]


writeToFile :: IO ()
writeToFile = TIO.writeFile "books.html" (booksToHtml  myBooks)



-- now working with MARCs
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

-- extracting the leader from the record
-- the leader is 24 bytes
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

{-
The first 5 bytes of the leader contain 
a number telling you the length of the record.

 To get the length of your entire record, 
 you need to take these first five characters 
 and then convert them to an Int type. 
 You’ll create a useful helper function, rawToInt, 
 which will safely convert your ByteString to Text, 
 then convert that Text to a String, and finally use read to parse an Int.
-}

rawToInt :: B.ByteString -> Int 
rawToInt byte = (read . T.unpack . E.decodeUtf8) byte


getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)