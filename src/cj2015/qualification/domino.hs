import Data.List
import Control.Monad
import Data.Function

type Case = [Int]
type Solution = String

solve :: Case -> Solution
solve (x:y:z:_) = name (func2 x y z)

name (area, row, cols)
	| area && (row || cols) = "GABRIEL"
	| otherwise = "RICHARD"


func :: Int -> Int -> Int -> (Bool,Bool)
func d r c = ((eRows && canRows), (eCols && canCols))
	where
		definite = d < r && d < c
		fitRows = d <= r 
		fitCols = d <= c
		eRows = mod (mod d r) 2 == 0
		eCols = mod (mod d c) 2 == 0
    
func2 :: Int -> Int -> Int -> (Bool,Bool,Bool)
func2 d r c = ( areaFits, canRows && eRows, canCols && eCols)
	where
		canRows = d <= r 
		canCols = d <= c
		areaFits = d*2 <= (r*c)
		eRows = mod (mod d r) 2 == 0
		eCols = mod (mod d c) 2 == 0

formatSolution :: Solution -> String
formatSolution sol = sol


readCase :: IO Case
readCase = do
	line <- getLine
	return (map read (words line))


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

