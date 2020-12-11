toPart recipient = "Dear " ++ recipient ++ "\n"

fromPart title = " Thank you for buying the book " ++ title

authorPart author = "Thanks, " ++ author

createMain recipient title author = toPart recipient ++ fromPart title ++ authorPart author



main :: IO()

main = do 
    print ("Enter name ")
    name <- getLine
    print ("Enter title ")
    bookTitle <- getLine
    print ("Enter author")
    author <- getLine
    print ( createMain name bookTitle author)
    

