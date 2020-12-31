-- I/O

main :: IO()
main = do
       putStrLn "hi"
       name <- getLine
       putStrLn name