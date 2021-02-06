--import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word

bcInt :: L8.ByteString
bcInt =   L8.pack "abc" 


-- definiing a type to contain the "state" of the parsing operation
-- the amount of string that is "left" to be parsed,
-- how much of the string have we parsed
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)


-- parsing as a kind of function: it consumes a parsing state, 
-- and produces both a new parsing state and some other piece of information

-- takes the state of the parsing, and perhaps does a parse, returns an updated state with _some_ more info -- can be anything, 
-- i think 'a' is the type of output we are expecting from the parser
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined


-- a Parse data type, which is a container type, 
---- that contains a function (a "parser" which takes in a ParseState and returns an  Either String (a, ParseState))
-- the type Parse is of kind * -> *  
-- this can act as a wrapper around simpleParse or betterParse
-- what is "a"? "a" seems to be the "result" of what has been parsed till now.
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

-- this is a "Parse" generator,
-- this will return us a Parse object basically only
-- as we know that Parse objects have a parser (ParseState -> Either Str (a,ParseState)) inside them
-- for this idenity Parse generator, the function that this wraps areound, returns the new ParseState as the same as it was before.
-- i.e does not change the ParseState
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))   
-- identity is like the ```Just```, except slightly more complex
    -- just like how Just is a wrapper around any type
        -- similarly, identity helps us create a wrapper (of Parse) around a type
            -- they're both an injector: inject :: a -> m a


-- the parse function,
-- it takes a Parse object (i.e ones that contain those parser functions)
-- and it takes a bytestring,
-- and then it returns _something_ idk what does "Either String a"

-- how to use it to parse something?
-- we get the function contained inside Parse object, by running (runParse parser)
-- now that we have the parser, we can give it a ParseState, (ParseState initState 0)
-- the specific parser obtained on doing (runParse parser) will now use its own logic to handle and parse ParseState
-- it has two possible outputs Left and Right
-- for the right, remember what the function inside parse returns, a Right (a, ParseState),
-- the a signifies what has been parsed, the function returns that inside the Right data constructor.
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = 
    case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (result,_) -> Right result
-- more info on the above ^,
-- 1. we must do is peel off the Parse wrapper so that we can get at the function inside.
-- 2. We do so using the runParse function. ie. ```runParse Parser```. 
-- 3. We also need to construct a ParseState, then run our parsing function on that parse state.

------------------------------
------------------------------
-- how to modify state?
-- 1. modifying offset in ParseState
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }        

-- how to get state of a ParseState?
-- remember Parse?
-- it was a type class which had an input.
-- it used to contain a function (which was a parser), 
-- the function would return Either String (a,ParseState) and accept ParseState
-- if we want a ParseState to return it's state -- we need a ParseState -> Either String (ParseState, ParseState)
-- that can be generated here,
-- this is a partial function, 
-- it is wrapped inside Parse: it contains a lambda which accepts a ParseState and returns an Either
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))
-- this can get confusing as the type signature might seem to  reflect: this is not a ParseState wrapped in Parse
-- Parse can not wrap a ParseState, Parse can only wrap a function in it's context,
-- the function that it wraps returns an Either String (ParseState, ParseState), which is what Parse ParseState represents

--putState is not a partial function,
-- it takes in a ParseState, 
-- and returns a Parse which contains a function ("parser") that takes anything and returns the state (ie ParseState) that was "put"
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
-- again note, we are taking a state, 
-- we are creating a new function, that on giving some input would return the desired state.


-- for reporting an error in parsing.
bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err
-- Takes an error message, 
-- and returns a function (parser) wrapped in a Parse.
-- which on taking any ParseState would return an error message (Left )



-- The (==>) function serves a similar purpose to our earlier (>>?) function: it is “glue” that lets us chain functions together. No comments 

-- firstParser is a function wrapped inside Parse
-- we obtain the actual function by doing, (runParse firstParser)
-- then we appply it to the initState (which will be a ParseState)
-- IF the logic inside firstParser is not able to parse it we reutrn an error,
-- if it works, we have successully obtained the results so far (firstResult), 
    -- and the next thing we wanna parse is the remaining ParseState i.e. (parseState)
-- we gotta tell the secondParser what we have till now, it generates for us a Parse b
-- Parse b is another wrapper around a function ("parser") which contains some logic.
-- to obtain the parser function we do a runParse (secondParser firstResult) to obtain the function
-- to tthis newly  
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                    Left errMessage -> Left errMessage
                    Right (firstResult, parseState) -> runParse (secondParser firstResult) parseState
-- THIS operator seems to be transferring the state from one parser to next
-- the 2nd operation is precisely to take the first state and send to the next parsing.
-- this operator automatically applies the logic of the second parser as well


-- Parsing a single byte,
parseByte :: Parse Word8 -- means, that we return a parser. 
-- big picture this means that parseByte is of a type Parse Word8
 -- which means that it contains a function inside it which is a parser function
    -- if you extract out this function, (using ```parseRun parseByte```), 
        -- you'll have a function which can take in a ParseState and return an Either String (Word8, ParseState)

-- explanation:
-- getState is a ```Parse ParseState```, it takes in a ParseState  (so that parseByte can take in a parse state) and returns the Either String (..,..)
-- now (like a good monadic function, very similar to bind )this state gets sent to the next (2nd) function.
-- the 2nd function, 
-- takes the first element from string of the  the initState (:t ParseState)
-- if there is nothing, then we bail
-- else, we modify the state: extract out 1st element.
    -- by obtaining the rest of the string (remainder), 
        -- and increasing the offset by 1 [[all this goes into the newState]]

-- we put the newState in putState (so we can convert our ParseState into Parse () ) because needed for it being a proper monad.
    -- this Parse () now becomes the input for the next element in the chain. just as needed.

-- next, now that this function also returned a Parse (),
    -- we chain ()==>) it with another function which can accept a ParseState and return a Parse,
       -- so, it returns an ```identity byte```
            -- ( remember that byte contained our original first byte that we parsed) 


-- see the type of the 3rs function (\_ -> identity byte) 
    -- is:  ```a -> Parse b```, 
        -- this needs a type as an input, this becomes our ParseState.
            --  due to the nature of ==> operator, the ParseState is extracted out
              ---  the desugared code is  ```runPrser ((\_ -> identity byte) newState) ``,

-- basically again, this can accept a ParserState and then run other operations on it.
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1



---------------------------------
--------------------------------
-- Example:
--  runParse (identity "aditya") (ParseState (L8.pack "100") 0)
-- > Right ("aditya",ParseState {string = "100", offset = 0})


-- we need to do this step because parseByte is literally a CONTEXT, it is Parse Word8
-- doing this extracts the function out
myByteParser = runParse parseByte 
-- ^ its type: myByteParser::  ParseState -> Either String (Word8, ParseState)

-- now we can give it some parseState, it will give you back the same parseState and having reutnred you the first byte
-- first creating parseState
myParseState = ParseState (L8.pack "aditya") 0
-- next a parser for that state
parseMyByte = myByteParser myParseState


main :: IO ()
main = do
  let contents = L8.pack "a my name is aditya"
  let initState = ParseState contents 0
  let parse_result = runParse parseByte initState
  case parse_result of
    Left error -> putStrLn $ "Error parsing file: " ++ error
    Right (word, parse_state) -> putStrLn $ "Byte parsed: " ++ show word
  return ()


  -- more examples,
myParser01 = identity (L8.pack "a_sentence_to_parser")
myRawParserFunction = runParse myParser01  -- extract the function out
output_ =  myRawParserFunction (ParseState bcInt  0)



{-
Three properties that make the Parse type a monad are

For Parse, the corresponding properties are 
1. the type constructor Parse a, 
2. the chaining function (==>), 
3. and the injector function identity. 


if we wanna make Parse a monad _formally_,
    we can do something like this:

instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail    
    
-}