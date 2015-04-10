import Data.List
import Control.Monad
import Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as May

type Case = [String]
type Solution = [String]

solve :: Case -> Solution
solve (x:y:_) = map isRecycled a end | a <- arange)
	where
		end = read y :: Int
		start = read x :: Int
		arange = [start .. end]


isRecycled (x,y) = if valid then swapped ++ "-" ++ a else ""--if valid then 1 else 0
	where
		a = show x
		b = show y
		breakChar = b !! 0
		(l,r) = break (== breakChar) a
		swapped = r++l
		valid = swapped == b


formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	line <- getLine
	return (words line)
	--pairs = [(x,y) | x <- [start..end] y <- [start..end]]
	--return pairs
	--return [("10", "40")]


readCases :: IO [Case]
readCases = do
	n <- getLine
	res <- replicateM (read n) readCase
	return res


main = do
	cases <- readCases
	
	let	
		solutions = map solve cases
		formattedSolutions = map formatSolution solutions
		output = zipWith (++) prefixes formattedSolutions
	--print $ cases
	putStr $ unlines output

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]



