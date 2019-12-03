import System.IO

getFuel moduleMass =
	let cost = (div moduleMass 3) - 2
	in if cost > 0
		then cost + (getFuel cost)
		else 0

main = do
	inputFile <- openFile "input" ReadMode
	input <- hGetContents inputFile

	let values = [read x :: Integer | x <- (lines input)]
	let total = sum [getFuel x | x <- values]

	putStr (show total)
