
-- TAKE an input as a list, add an element to all elements and output it to the workd
--test :: [Int] -> IO ()
test givenList = do 
                 element <- givenList
                 let newEle  = element + 1 
                 return (show newEle)

test2 :: [Int] -> IO [Int]
test2 givenList = do 
                  return ( givenList)


test3 :: [Int] -> IO [Int]
test3 givenList = do 
                  let updatedList = map (+1) givenList
                  return updatedList

test4 givenList = do
                  extractFromIO <-    test2 (givenList)
                  return (operatedOnList (extractFromIO)) -- this return will put this list back into IO
                  where operatedOnList mylist = map (+1) mylist
                


test5 givenList = do
                  extractFromIO <-    test2 (givenList)
                  return (operatedOnList (extractFromIO)) -- this return will put this list back into IO
                  where operatedOnList mylist = do 
                                                value <- mylist
                                                let newValue = value + 1
                                                return newValue
                


          
                                