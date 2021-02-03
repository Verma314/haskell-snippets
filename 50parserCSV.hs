import Text.ParserCombinators.Parsec
-- :m Text.ParserCombinators.Parsec
csvFile = endBy line eol
line = sepBy cell (char ',') -- line parses input till it reaches 
cell = many (noneOf ",\n")  -- parses input till it encounters a "," or "\n"


eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

eol_old :: GenParser Char st Char
eol_old = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

eol2 :: GenParser Char st String
eol2 = (string "\n") <|> (string "\n\r")  

eol3 :: GenParser Char st Char
eol3 = 
    do char '\n'  -- first try parsing \n, if not then the next one:
       char '\r' <|> return '\n' 

{-
This function first looks for \n.
 If it is found, then it will look for \r, 
 consuming it if possible. 
Since the return type of char '\r' is a Char, the alternative action is to simply return a Char without attempting to parse anything

-}
{-

*Main> parse (eol3 >> eof) "" "\n\r"
Right ()
-}       