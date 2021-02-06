import Text.ParserCombinators.Parsec
--import Text.Parsec.Prim.Parsec
-- :set -package parsec


csvFile = sepBy line (char '\n')
line = sepBy cell (char ',') -- this owuld get the entire line
cell = many (noneOf ",\n")  -- this gets one cell from the line
--eol = char '\n'


parseCell :: String -> Either ParseError [Char]
parseCell input = parse cell "(unknown)" input
-- > parseCell "1234,234,234"
-- Right "1234"
parseString :: String -> Either ParseError [String]
parseString input = parse line "(unknown)" input
-- > parseString "1234,234,234"
-- Right ["1234","234","234"]

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
-- > parseCSV "123,123,123\n123,123\n"
-- Right [["123","123","123"],["123","123"]]

{-
parseLines :: String -> Either ParseError [[String]]
parseLines input = parse line "(unknown)" input
-}
{-
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
-}

{-

this works
--csvFile = endBy cells (char '\n')
--line = sepBy cells (char '\n')
cells = sepBy (many (noneOf ",\n")) (char ',')
--eol = char '\n'


-}

--myParser :: Text.Parsec.Prim.Parsec String () String 
myParser = try (string "aditya" >> return "is cool")  
       <|> try (string "awesom" >> return "is also cool")
       <|> fail "Couldn't find coolness"
-- works on both aditya and awesome,
-- why "try"? because without which 
 -- <|> would only attempts the option on the right if the option on the left consumed **no input.**

myParser2 = try (string "aditya" >> return "is cool")  
       <|> try (string "awesom" >> return "is also cool")
       <?> "Couldn't find coolness"       