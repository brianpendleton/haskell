import qualified Data.Set as S
import Data.List
import Control.Monad
import Data.Function

type Case = [Int]
type Solution = Int


solve :: Case -> Solution
--solve x = length x - length (solve' 0 x)
--solve x = length x - maximum ( map length (solve' x))
solve x = maximum (map length (solve' (reverse [snd x | x <- sortBy (compare `on` fst) (zip x [1..])])))
--solve x = reverse [snd x | x <- sortBy (compare `on` fst) (zip x [1..])]


f lst (x:xs)
	| x > lst = x: (f x xs)
	| otherwise = f lst xs
f lst _ = []

solve' (x:xs) =	f x (x:xs) : solve' xs
solve' _ = [[]]



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
