import System.IO



main = do
    inputFile <- openFile "input" ReadMode
    input <- hGetContents inputFile

    putStrLn input
