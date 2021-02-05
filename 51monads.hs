--import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
bcInt :: L8.ByteString
bcInt =   L8.pack "abc" 

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)


-- parsing as a kind of function: it consumes a parsing state, 
-- and produces both a new parsing state and some other piece of information
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined


-- the type Parse is of kind * -> *  contains one function, which is of the type ```ParseState -> Either String (a, ParseState)```
-- ie the same time as "betterParse"
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }


---The identity parser generator
-- it generates the parser for tthe type a, wrapped insifde Parse
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))   


-- how to use it to parse something?
-- 1. we must do is peel off the Parse wrapper so that we can get at the function inside.
-- 2. We do so using the runParse function. ie. ```runParse Parser```. 
-- 3. We also need to construct a ParseState, then run our parsing function on that parse state.
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = 
    case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (result,_) -> Right result


-- modify offser in ParseState
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }        