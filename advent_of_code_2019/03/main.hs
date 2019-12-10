import System.IO


data Direction = Up | Right | Down | Left
    deriving (Show)

type Move = (Direction, Int)
type Location = (Int, Int)
type Line = (Location, Location)


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseMove :: String -> Move
parseMove s =
    case dir of
        'U' -> (Main.Up, dist)
        'R' -> (Main.Right, dist)
        'D' -> (Main.Down, dist)
        'L' -> (Main.Left, dist)
    where
        dir = head s
        dist = read (tail s) :: Int

move :: Location -> Move -> Location
move start m =
    case dir of
        Main.Up     -> (x, y + dist)
        Main.Right  -> (x + dist, y)
        Main.Down   -> (x, y - dist)
        Main.Left   -> (x - dist, y)
    where
        (x, y)      = start
        (dir, dist) = m


data Orientation = Horizontal | Vertical
    deriving (Show)

data Intersect = DidIntersect Location | DidNotIntersect
    deriving (Show)


orientation :: Line -> Orientation
orientation line
    | x1 == x2 = Vertical
    | y1 == y2 = Horizontal
    where
        (x1, y1) = fst line
        (x2, y2) = snd line


in_range :: Int -> (Int, Int) -> Bool
in_range v (min, max)
    | min > max = in_range v (max, min)
    | otherwise = v > min && v < max

intersect_hv :: Line -> Line -> Intersect
intersect_hv horizontal vertical =
    let v_x = (fst (fst vertical))
        h_y = (snd (fst horizontal))
    in
        if
            in_range v_x (fst (fst horizontal), fst (snd horizontal)) &&
            in_range h_y (snd (fst vertical), snd (snd vertical))
        then
            DidIntersect (v_x, h_y)
        else
            DidNotIntersect

intersect :: Line -> Line -> Intersect
intersect l1 l2 =
    case (o1, o2) of
        (Horizontal, Vertical) -> intersect_hv l1 l2
        (Vertical, Horizontal) -> intersect l2 l1
        (Horizontal, Horizontal) -> DidNotIntersect
        (Vertical, Vertical) -> DidNotIntersect
    where
        o1 = orientation l1
        o2 = orientation l2


foldMoves :: [Move] -> Location -> [Line]
foldMoves [last] prev =
    [(prev, move prev last)]

foldMoves moves prev =
    let m = head moves
        t = tail moves
    in
        (prev, move prev m) : foldMoves t (move prev m)


fold :: ((b, a) -> b) -> b -> [a] -> b
fold func init [] = init
fold func init list =
    fold func (func (init, head list)) (tail list)


bestIntersect :: Int -> Line -> Line -> Int
bestIntersect prevBest l1 l2 =
    case intersect l1 l2 of
        DidNotIntersect -> prevBest
        DidIntersect (x, y) -> min (abs (x + y)) prevBest

main = do
    inputFile <- openFile "input" ReadMode
    input <- hGetContents inputFile

    let l = lines input
    let w = [wordsWhen (== ',') x | x <- l]
    let commands = [map parseMove x | x <- w]

    let start = (0, 0)
    let end = move start (Main.Right, 15)

    let s2 = (3, 3)
    let e2 = move s2 (Main.Down, 6)

    let l1 = foldMoves (head commands) start
    let l2 = foldMoves (head (tail commands)) start


    let best = fold (\(best, s1) -> fold (\(best, s2) -> bestIntersect best s1 s2) best l2) 123456789 l1

    putStrLn (show best)
    -- putStrLn (show (foldMoves (head commands) start))
