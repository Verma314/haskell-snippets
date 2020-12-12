import Data.List

author = ("Aditya","Verma")

testFxn = do
        print ( fst author )
        print ( snd author)

testFxn2 = sort [10,9,8,8,7,6]
testFxn3 = sort ["Aditya", "Verma", "Zzzz", "Aaaaa"]



compareByFxn elem1 elem2 = if elem1snd > elem2snd
                            then GT
                            else if elem1snd < elem2snd
                            then LT
                            else if elem1fst > elem2fst
                                then GT
                                else if elem1fst < elem2fst
                                    then LT
                                    else
                                        EQ

                        where elem1snd = snd elem1
                              elem2snd = snd elem2
                              elem1fst = fst elem1
                              elem2fst = fst elem2

testFxn4 =   do
            let mylist = [ ("babc","babc"), ("abcd","abcd"), ("d","d")]
            sortBy compareByFxn  mylist               


testFxn5 givenList = sortBy compareByFxn givenList    


 