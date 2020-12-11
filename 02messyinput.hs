messyMain :: IO()

messyMain = do
    print "Who is the recipient?"
    recipient <- getLine    
    print "What is the title"
    title <- getLine
    print "Who is the author"
    author <- getLine
    print ( "Dear " ++ recipient ++ " \n Thank you for buying " ++ title ++ " ++ written by " ++ author  )







quicksort [] = []
quicksort (x: xs) = quicksort ( lesser ) ++ [x] ++ quicksort (greater)
    where 
        lesser =  filter ( < x ) xs
        greater = filter ( >= x) xs  




main :: IO()
main = do
    print "do nothing"
