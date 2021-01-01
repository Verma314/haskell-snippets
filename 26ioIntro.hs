-- I/O

hello :: String -> String
hello name = "Hello " ++ name

main :: IO()
main = do
       putStrLn "hi"
       name <- getLine --  <- is used because getLine returns an IO String , but we want to use name like a regular String
       let statement = hello name -- <- you have to use let whenever we create variables that AREN'T IO type
       putStrLn (hello name)
