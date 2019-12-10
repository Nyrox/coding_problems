import System.IO
import Data.List
import Data.Tuple
import Data.Maybe

import Debug.Trace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Program = [(Int, Int)]

insertCell :: Program -> Int -> Int -> Program
insertCell program p v =
    (p, v) : program

initCells :: [Int] -> Int -> Program
initCells [] offset = []
initCells values offset =
    (offset, head values) : (initCells (tail values) (offset + 1))

initProgram :: String -> Program
initProgram input =
    let cells = [read x :: Int | x <- wordsWhen (==',') input]
    in
        initCells cells 0

readCell :: Program -> Int -> Int
readCell program offset =
    let cell = find (\t -> (fst t) == offset) program
    in
        snd (fromJust cell)

writeCell :: Program -> Int -> Int -> Program
writeCell program address value =
    (address, value) : program


type Computer = (Program, Int)

data StepResult = Continue Computer | NeedInput (Int -> Computer) | Output Computer Int | Halt 

type ParameterMode = Int


data Operator = Operator { opcode :: Int
                , pmode1 :: ParameterMode
                , pmode2 :: ParameterMode
                , pmode3 :: ParameterMode
                } deriving (Show)


parseOperator :: Int -> Operator
parseOperator opval =
    Operator {
        opcode = opval `mod` 100,
        pmode1 = (opval `mod` 1000) `div` 100,
        pmode2 = (opval `mod` 10000) `div` 1000,
        pmode3 = (opval `mod` 100000) `div` 10000 
    }

step :: Computer -> StepResult
step (program, isp) =
    let operator = parseOperator $ readCell program isp
        readParamMode = \(offset, mode) -> case mode of 
            0 -> readCell program $ readCell program (isp + offset)
            1 -> readCell program (isp + offset)
        
        readParam = \offset -> case offset of
            1 -> readParamMode (offset, pmode1 operator)
            2 -> readParamMode (offset, pmode2 operator)
            3 -> readParamMode (offset, pmode3 operator)

    in case opcode operator of
        99 -> Halt
        1 -> 
            let lhs = readParam 1
                rhs = readParam 2
                out = readCell program (isp + 3)
            in Continue (writeCell program out (lhs + rhs), isp + 4)
        2 ->
            let lhs = readParam 1
                rhs = readParam 2
                out = readCell program (isp + 3)
            in Continue (writeCell program out (lhs * rhs), isp + 4)
        3 ->
            let out = readCell program (isp + 1)
            in NeedInput (\input -> (writeCell program out input, isp + 2))
        4 ->
            let val = readParam 1
            in Output (program, isp + 2) val

        otherwise -> error $ "unknown opcode: " ++ show operator

exec :: Computer -> IO () 
exec computer = do
    let res = step computer
    
    case res of
        Halt -> 
            trace "halt"
            return ()
        Output comp val -> do
            putStrLn $ show val
            exec comp
        Continue comp -> exec comp
        NeedInput cb -> do
            input <- getLine
            let iv = read input :: Int
            exec $ cb iv

main = do
    inputFile <- openFile "input" ReadMode
    input <- hGetContents inputFile

    let p1 = initProgram input

    exec (p1, 0)

