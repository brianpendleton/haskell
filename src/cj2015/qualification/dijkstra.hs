import Data.List
import Control.Monad
import Data.Function

type Case = String
type Solution = [String]

--solve :: Case -> Solution
solve x = x

f (x:y:xs) needed
	| result == "i" = 

mult x y 
	| x == "1" && y == "1" = "1"
	| x == "1" && y == "i" = "i"
	| x == "1" && y == "j" = "j"
	| x == "1" && y == "k" = "k"
	| x == "i" && y == "1" = "i"
	| x == "i" && y == "i" = "-1"
	| x == "i" && y == "j" = "k"
	| x == "i" && y == "k" = "-j"
	| x == "j" && y == "1" = "j"
	| x == "j" && y == "i" = "-k"
	| x == "j" && y == "j" = "-1"
	| x == "j" && y == "k" = "i"
	| x == "k" && y == "1" = "k"
	| x == "k" && y == "i" = "j"
	| x == "k" && y == "j" = "-i"
	| x == "k" && y == "k" = "-1"
	| otherwise = ""
	

--formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	line <- getLine
	let (size:nums:_) = (map read (words line)) 
	line <- getLine
	return (take (size*nums) (cycle line))


readCases :: IO [Case]
readCases = do
	n <- getLine
	res <- replicateM (read n) readCase
	--res <- replicateM 1 readCase
	return res


main = do
	cases <- readCases
	let	
		solutions = map solve cases
		formattedSolutions = map formatSolution solutions
		output = zipWith (++) prefixes formattedSolutions
	putStr $ unlines output

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]


-- 1 3 5 2
-- 1 2 3 2 3
-- 0 1 2 1 2
-- 0 0 1 0 1
-- 0 0 0 0 0

