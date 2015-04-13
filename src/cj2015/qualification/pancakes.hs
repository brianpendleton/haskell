import Data.List
import Control.Monad
import Data.Function

type Case = [Int]
type Solution = Int

--solve :: Case -> Solution
solve x = f x 0


f :: [Int] -> Int -> Int
f (x:xs) cnt
	| currMax > maxSpec = f special cnt+1
	| otherwise = f (eat orig) cnt+1   
	where
		orig = x:xs
		s = sort (orig)
		currMax = last s
		special = (init s) ++ specialFlip currMax
		maxSpec = maximum special
f _ cnt = cnt

specialFlip :: Int -> [Int]
specialFlip x = let d = div x 2 in [d, x-d]


eat :: [Int] -> [Int]
eat x = filter (\x -> x>0) (map (subtract 1) x)


formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	garbage <- getLine
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

