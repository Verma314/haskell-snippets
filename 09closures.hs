ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n * 2
                 else n




-- functions as arguments:
ifEvenApplyFunc n func = if even n
                         then func n
                         else n

isEvenInc2 n = ifEvenApplyFunc n (\x -> x + 1 )

-- What we want is a function that builds ifEvenX functions.
genIfEvenX myCustomFunc =  ( \x -> ifEvenApplyFunc x myCustomFunc ) 

--  a function genIfXEven that creates 'a closure with x' and returns a new function that allows the user to pass in a function to apply to x if x is even.
genIfXEven x = (\ givenFunc -> ifEvenApplyFunc x givenFunc)



-- write it all from scratch

myinc =  \x -> x + 1 

-- write ifEvenThenApply func using a closure on func
ifEvenApplyFuncClosure func = (\ x -> if even x then func x else x)
-- even using this function alone would do ^, takes a custom function

ifEvenThenApplyFunc func = (\ x -> if even x then func x else x)
--


---

-- Exercise from Will Kurt's book Get Programming in Haskell

-- should be gen*
getRequestURL host apiKey resource id = host ++ "/" ++ apiKey ++ "/" ++ resource ++ "/" ++ id


--capturing the host value in a closure
genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)

amazonHostBuilder = genHostRequestBuilder "amazon.com" -- this is creating a closure
googHostBuilder = genHostRequestBuilder "google.com"

 
genApiKeyRequestBuilder hostBuilder apiKey = (\ resource id -> hostBuilder apiKey resource id ) -- this can create more closures like

amazonHostBuilderWithApiKey1 = genApiKeyRequestBuilder amazonHostBuilder "1"
googleHostBuilderWithApiKey2 = genApiKeyRequestBuilder googHostBuilder "2" --this method is closure in which the hostbuilder and api key are already set

--we can use this to build another closure-creator:
-- this one will also be able to build closures which have a fixed resource 
-- and only need to input an id
genApiKeyRequestBuilder2 hostApiKeySetClosure resource = (\ id -> hostApiKeySetClosure resource id)

genApiKeyRequestBuilder2Alternative genApiKeyRequestBuilder resource = (\ apikey id  -> genApiKeyRequestBuilder )


---------------- practising closures again ---------------------

generateObject root level1 level2 level3 = root ++ " " ++ level1 ++ " " ++ level2 ++ " " ++ level3

-- no closure:

generate_01 = generateObject "level 0" "level 1" "level 2" "level 3"

--clousure with level 0 set already

--closure to set level0 to something
generateObjectLevel0set level0 = (\level1  level2 level3 -> generateObject level0 level1 level2 level3)


--set root as "ROOT"
generate_02level0Root = generateObjectLevel0set "root"
generate_03level0RootX = generateObjectLevel0set "rootx"


-- close to already set level0 and level 1 to something.

--should we use the previous closure with level0 set? or should we set it independantly
--independantly is easy and makes much more sense 
-- but lets try using the previous closure also
-- in this the user can give us a closure in which level0/root is already set

--this is to generate closures 
generateObjectLevel0and1set closureWithLevel0Set level1 = (\level2 level3 -> closureWithLevel0Set level1 level2 level3)

generate_04level0rootlevel1Random = generateObjectLevel0and1set generate_02level0Root "random"

---

-- create closure which can fix your level 0, 1,2 

generateObjectLevel01and2Set closureWithLevel0and1set level2 = (\level3 ->   closureWithLevel0and1set level2 level3 )

--generate_05level012set = generateObjectLevel01and2Set generate_04level0rootlevel1Random "level 2"