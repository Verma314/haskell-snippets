{-# LANGUAGE OverloadedStrings #-}
import System.IO

-- openFile :: FilePath -> IOMode -> IO Handle
openAndWrite = do 
        myFile <- openFile "hello.txt" ReadMode
        firstLine <- hGetLine myFile
        putStrLn firstLine
        secondLine <- hGetLine myFile

        -- open 2nd file for writing
        myOutFile <- openFile "hello2.txt" WriteMode
        hPutStrLn myOutFile secondLine

        -- close the files
        hClose myFile
        hClose myOutFile
        putStrLn "done!"




-- check if file is empty
readTillEof :: IO ()
readTillEof = do    
                randomFile <- openFile "randomfile.txt" ReadMode
                reachedEOF <- hIsEOF randomFile
                readValues <- if not reachedEOF
                              then hGetLine randomFile
                              else return "empty"
                putStrLn readValues                              
                putStrLn "Done!"


-- check if 2nd line is empty
secondLineEmpty :: IO ()
secondLineEmpty = do    
                randomFile <- openFile "randomfile.txt" ReadMode
                readLineOne <- hGetLine randomFile
                --has it now reached EOF?
                reachedEOF <- hIsEOF randomFile
                readValues <- if not reachedEOF
                              then hGetLine randomFile
                              else return "2nd line empty"
                putStrLn readValues                              
                putStrLn "Done!"


-- get counts
getCounts :: String -> (Int, Int,Int)
getCounts input = (wordc, linec, chars)
                    where wordc = (length . words) input
                          linec = (length . lines) input
                          chars = length input

countToText :: (Int, Int, Int) -> String
countToText (wc,lc,c)= unwords ["words : ", show wc, " lines : ", show lc , " chars : ", show c]


-- now do this with a file
getInfoOfFile :: String -> IO ()
getInfoOfFile fileName = do
                            input <- readFile fileName
                            let values = (countToText . getCounts) input
                            -- lets append this to our stats file
                            appendFile "stats.dat" (mconcat [ fileName , "          ", values , "\n" ])



-- todo last 2 exercises in this chapter