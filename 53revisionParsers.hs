import Text.ParserCombinators.Parsec
-- :set -package parsec




-- GenParser context, it reads individual Chars and returns out lists of Strings.
csvFile :: GenParser Char st [[String]]
csvFile =  do
           result <- many line
           eof
           return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = do
       result <- cells  
       eol
       return result

-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
-- this gets all the cells in a line
cells :: GenParser Char st [String]
cells = do
        first <-  cellContent
        next <- remainingCells
        return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more ce

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'                