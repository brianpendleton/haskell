import qualified Data.Set as S
import Data.List
import Control.Monad
import Data.Function

type Case = [Int]
type Solution = Int


solve :: Case -> Solution
solve x = length x - (length $ foldl f [] x)

f (x:xs) n
	| n > x = x:(f xs n)
	| otherwise = n:xs
f _ n = [n]


formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	n <- getLine
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
