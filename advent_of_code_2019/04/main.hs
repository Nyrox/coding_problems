

fold :: ((b, a) -> b) -> b -> [a] -> b
fold func init [] = init
fold func init list =
    fold func (func (init, head list)) (tail list)



hasAdj digits
    | length digits >= 2 = (
        if head digits == head (tail digits) then
            True
        else
            hasAdj (tail digits))
    | otherwise = False

isAscending digits
    | length digits >= 2 = (
        if head digits > head (tail digits) then
            False
        else 
            isAscending (tail digits))
    | otherwise = True

valid :: Int -> Int -> Int -> Bool
valid min max p = 
    let s       = show p
        digits  = [read [x] :: Int | x <- s]
    in
        length digits == 6 && p < max && p > min && hasAdj digits && isAscending digits

bruteForce :: Int -> Int -> Int
bruteForce min max =
    fold (\(s, i) -> if valid min max i then s + 1 else s) 0 [min..max]


main = do
    let min = 193651
    let max = 649729


    putStrLn (show (bruteForce min max))
