-- for theory behind either, see the Readme.md

{-
> (+12) <$> (Right 101)
Right 113

> (+12) <$> (Left 101)
Left 101

> (+12) <$> (Left "errrorrr!")
Left "errrorrr!"

-}

-- data Either a b = Left a | Right b

-- Either String a , means Left constructor takes a string, right constructor takes a
eitherHead :: [a] -> Either String a
eitherHead [] = Left "Can't return head -- List Empty!"
eitherHead (x:xs) = Right x -- used pattern matching instead of using the unsafe head

{-
> eitherHead [1..]
Right 1
>
>
> eitherHead []
Left "Can't return head -- List Empty!"

-}

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""


{-
Use <$> and <*> to add the first and second numbers in intExample by using eitherHead.

intExample = [1,2,3]
-}

valz =  (+)  <$>  (eitherHead intExample) <*> (eitherHead (tail intExample))

addTwoValuesList :: [Int] -> Either String Int
addTwoValuesList myList =  (+)  <$>  (eitherHead myList) <*> (eitherHead (tail myList))

{-
*Main> addTwoValuesList [1,2,3]
Right 3
*Main> 
*Main> addTwoValuesList [1]
Left "Can't return head -- List Empty!"
*Main> 
*Main> addTwoValuesList []
Left "Can't return head -- List Empty!"
*Main> 

very amazing.

-}