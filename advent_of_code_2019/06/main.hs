
import System.IO
import Data.Tuple
import Data.Maybe
import Data.List as L

import Debug.Trace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type OrbitPair = (String, String)
data Tree a = Node a [Tree a]
    deriving (Show)

type OrbitMap = Tree String

leaf :: String -> OrbitMap
leaf name =
    Node name []

insertOrbit :: OrbitMap -> String -> String -> OrbitMap
insertOrbit tree parent what =
    case v == parent of
        True -> Node v (leaf what : c)
        False ->  Node v $ map (\i -> insertOrbit i parent what) c
    where
        Node v c = tree

countOrbits :: Int -> OrbitMap -> Int
countOrbits depth (Node _ [])   = depth
countOrbits depth (Node _ arr)  = (+) depth $ sum $ map (countOrbits (depth + 1)) arr

createOrbits :: [OrbitPair] -> String -> OrbitMap
createOrbits orbits parent =
    let childs = filter (\(p, _) -> p == parent) orbits
        childOrbits = map (createOrbits orbits) $ map snd childs
    in
        Node parent childOrbits

main = do
    inputFile <- openFile "testInput" ReadMode
    input <- hGetContents inputFile

    let _orbits = map (wordsWhen (== ')')) $ lines input
    let orbits = [(head x, head $ tail x) | x <- _orbits]
    let Just rootOrbit = L.find ((== "COM") . fst) orbits
    let orbitMap = Node (fst rootOrbit) [Node (snd rootOrbit) []]

    putStrLn $ show $ L.find ((== "COM") . fst ) orbits
    putStrLn $ show $ orbitMap
    putStrLn $ show $ countOrbits 0 $ createOrbits orbits "COM"
