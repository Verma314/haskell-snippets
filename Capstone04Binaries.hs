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


-- now we want to separate out the records from 
-- the entire file
separateRecords :: B.ByteString -> (MarcRecordRaw,  B.ByteString)
separateRecords bytes = B.splitAt  lengthOfRecord bytes
    where lengthOfRecord = getRecordLength bytes


allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = separateRecords marcStream


readMARC :: IO ()
readMARC = do
  marcData <- B.readFile "simple.mrc"
  let marcRecords = allRecords marcData
  print (length marcRecords)



type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory fullRecord = B.take directoryLength fullRecord
    where withoutLeader = B.drop leaderLength fullRecord
          directoryLength = getDirectoryLength (getLeader fullRecord)



type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12


--This is a fairly straightforward function: 
-- you take a chunk of 12 bytes and add them to a list until there’s no more list 
-- left.
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = firstMeta : restOfTheDirectory
    where (firstMeta,rest) = B.splitAt 12 directory 
          restOfTheDirectory = if directory == B.empty then []
                               else splitDirectory(rest)
    

data FieldMetadata = FieldMetadata { tag         :: T.Text
                                   , fieldLength :: Int
                                   , fieldStart  :: Int } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where (theTag,rest) = B.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength,rawStart) = B.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart = rawToInt rawStart

getFieldMetadata ::  [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

type FieldText = T.Text

{-
What you want now is to take a MarcRecordRaw, FieldMetadata 

and get back a FieldText so you can start looking up useful values!

To do this, 
you first have to drop both the leader and the directory from your MarcRecord 
so you end up with the base record. 

Then you can drop the fieldStart
from the record and finally take the fieldLength from this remaining bit.
-}

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
    where recordLength = getRecordLength (getLeader record)
          baseAddress = getBaseAddress (getLeader record)
          baseRecord = B.drop baseAddress record
          baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
          byteStringValue =  B.take (fieldLength fieldMetadata) baseAtEntry


--  extracting info out of the FieldText
fieldDelimiter :: Char
fieldDelimiter = toEnum 31
titleTag :: T.Text
titleTag = "245"
titleSubfield :: Char
titleSubfield = 'a'
authorTag :: T.Text
authorTag = "100"
authorSubfield :: Char
authorSubfield = 'a'
          
-- bored of this capsstone, to review later