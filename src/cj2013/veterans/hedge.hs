import qualified Data.Set as S
import Data.List
import Control.Monad

type Case = [Double]
type Solution = Double


solve :: Case -> Solution
solve (x:y:z:xs)
    | null xs = m
    | otherwise = solve (m:z:xs)
    where m = min y ((x+z)/2)
solve _ = 0.0


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
	return res


main = do
	cases <- readCases
	let	
		solutions = map solve cases
		formattedSolutions = map formatSolution solutions
		output = zipWith (++) prefixes formattedSolutions
	putStr $ unlines output

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]
