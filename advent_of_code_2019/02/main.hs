import System.IO
import Data.List
import Data.Tuple
import Data.Maybe

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Program = [(Integer, Integer)]

insertCell :: Program -> Integer -> Integer -> Program
insertCell program p v =
    (p, v) : program

initCells :: [Integer] -> Integer -> Program
initCells [] offset = []
initCells values offset =
    (offset, head values) : (initCells (tail values) (offset + 1))

initProgram :: String -> Program
initProgram input =
    let cells = [read x :: Integer | x <- wordsWhen (==',') input]
    in
        initCells cells 0

readCell :: Program -> Integer -> Integer
readCell program offset =
    let cell = find (\t -> (fst t) == offset) program
    in
        snd (fromJust cell)

writeCell :: Program -> Integer -> Integer -> Program
writeCell program address value =
    (address, value) : program

step :: Program -> Integer -> (Bool, Program, Integer)
step program isp =
    let opcode = readCell program isp
    in if opcode == 99
        then (True, program, 0)
        else
            let lhs = readCell program (readCell program (isp + 1))
                rhs = readCell program (readCell program (isp + 2))
                out = readCell program (isp + 3)
            in
                if opcode == 1
                    then (False, writeCell program out (lhs + rhs), isp + 4)
                else if opcode == 2
                    then (False, writeCell program out (lhs * rhs), isp + 4)
                else error "Cursed state reached"

runToEnd program isp =
    let (finished, nextProgram, nextIsp) = step program isp
    in
        if finished then nextProgram
        else runToEnd nextProgram nextIsp

runToEndWithParams program noun verb =
    let p1 = writeCell program 1 noun
        p2 = writeCell p1 2 verb
    in
        runToEnd p2 0

part1 :: Program -> Program
part1 p1 =
    let p2 = writeCell p1 1 12
        p3 = writeCell p2 2 2
    in
        runToEnd p3 0




solve1d :: Program -> Integer -> Integer -> Integer -> (Bool, (Integer, Integer))
solve1d program expect otherDim current =
    if current > 99 then (False, (0, 0))
    else
        let end = runToEndWithParams program otherDim current
            (0, res) = head end
        in if res == expect
            then (True, (otherDim, current))
            else solve1d program expect otherDim (current + 1)

solve2d :: Program -> Integer -> Integer -> (Bool, (Integer, Integer))
solve2d program expect current =
    if current > 99 then (False, (0, 0))
    else
        let (finished, res) = solve1d program expect current 0
        in if finished
            then (True, res)
            else solve2d program expect (current + 1)

solve :: Program -> Integer -> (Integer, Integer)
solve program expect =
    let (True, res) = solve2d program expect 0
    in
        res

main = do
    inputFile <- openFile "input" ReadMode
    input <- hGetContents inputFile

    let p1 = initProgram input

    putStrLn $ show $ part1 p1
    putStrLn $ show $ solve p1 19690720

