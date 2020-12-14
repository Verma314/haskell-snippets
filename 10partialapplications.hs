add4 a b c d = a + b + c + d  

-- usually what we would do:


addXto3 x = (\b c d ->
              add4 x b c d)


-- now we can pass a function a partial list of arguments, and it generates a closure (?) [not sure if i am using the right vocab here]
-- the new 'mystery' function generated has already captured one element in it, and will acceept the remaining 3 elements later when called              
mystery = add4 3

-- same as this

mysterySame = addXto3 3

-- this language feature is called partial application
mystery2 = add4 3 2 1 

mysterySame2 = mysterySame 1 2 3


-------------------------------------------------------------------------------------------------------------------

-------------------------------------------- Exercises Will Kurt 5.3.1 --------------------------------------------


createUrl host resource apiKey id = "http://" ++ host ++ "/" ++ resource ++ "/" ++ apiKey ++ "/" ++ id


googleHostBookResourceApiKeyRand = createUrl "google.com" "book" "42jh34bm23"
-- when doing a partial application you may choose to not send in any paramters to the function

googleHostBookResource apiKey = createUrl "google.com" "book" apiKey

--example, application:
uriCretorIdUnset = googleHostBookResource "qvehjq23v42" 

test = uriCretorIdUnset "1"

------------------------------------------------------------------------------------------------------------------------

-- swapping arguments: 

generateValue childVal parentVal = parentVal ++ " " ++ childVal

-- lets assume that the above is a library function that we cant edit:

generatedValuesSwapped parent child = generateValue child parent
-- created a partial-function-friendly function above ^
-- now we have a fxn which has an argument list from more to less generalized

-- lets test it out:
parentEquals1ChildUnset = generatedValuesSwapped "1"

test1 = parentEquals1ChildUnset "Child"


-- the above is a one time solution. But not scalable if we have n number of functions whose parameters need to be swapped

-- let us write a generalized function which can take any function, flip its arguments, and then return the new function

flipBinaryArgs myfunction = (\x y -> myfunction y x)

-- now lets fix our intiial function 
-- generateValue childVal parentVal = parentVal ++ " " ++ childVal

generateValueV2 = flipBinaryArgs generateValue

-- we can test it out
parentFixedAs1ChildUnset = generateValueV2 "1"


{--

flipBinaryArgs myfunction = (\x y -> myfunction y x)

This function 
1. is a closure because it takes in a function myFunction and 'traps' it as a paramter. The new resultant fxn can take it 2 params x, y
2. demonstrats 1st class functions. As this function accepts a function in the paramters 
3. uses lambda expressions.

-}

----------------------------------- Exercise 5.4 ------------------------------------------

{-
Use flip and partial application to create a function called subtract2 that removes 2 from whatever number is passed in to it.
-}

subtract_  a b = (-) a b 
flipSubtractParamters = flip subtract_
solution a = flipSubtractParamters 2 a

--- or

flipedNegativeSign = flip (-)
subtract2Alt = flipedNegativeSign 2 
test2 b = subtract2Alt b


--- or

flipedNegativeSign2 = flip (-) 2


--- 5.1 ----
{-
Now that you know about partial application, 
you no longer need to use genIfEvenX. Redefine ifEvenInc, ifEvenDouble, and ifEvenSquare by using ifEven and partial application.
-}

ifEven func x = if even x
                then func x     
                else x

ifEvenInc = ifEven ( \x -> x + 1)

ifEvenDouble = ifEven (\x -> x * 2 )


--
--or, without using partial functions
ifEvenCustom func = (\ x -> if even x then func x else x)
--


{-
write a function binaryPartialApplication that takes a binary function and one argument and returns a new function waiting for the missing argument.

-}

ifEvenCustom2 func = (\ x -> func x)
binaryPartialApplication func  = (\ x -> func x) 

-- function applies the argument, then the 2nd fxn created accepts the 2nd argument