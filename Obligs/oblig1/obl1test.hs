import System.IO
import Oblig1

readFromFile :: IO ()
readFromFile = do
    theFile2 <- openFile "gyldige-utrykk.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2

parseFile :: String -> IO [Ast]
parseFile fileName = do
    contents <- readFile fileName
    let parsed = map parse (lines contents)
    return parsed

--visFile :: String -> IO [String]
--visFile filename = do
--    contents <- readFile filename
--    let vised = map vis filename
--    return vised

--evalFile :: String -> IO [Ast] -> IO [String] 
--evalFile fileName = do
--    contents <- readFile fileName
--    let evaled = map eval (lines contents)
--    return evaled 

