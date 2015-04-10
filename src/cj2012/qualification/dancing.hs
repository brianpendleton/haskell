import Data.List
import Control.Monad
import Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as May

type Case = [Int]
type Solution = Int

solve :: Case -> Solution
solve (_:s:p:xs) = easyWins + ( min s surprises)
	where
		combos = divsRems xs
		easyWins = length $ filter isEasy combos
		surprises = length $ filter isSurprise combos
		isEasy (d:r:_) 
			| d >= p = True
			| (d + 1) == p && r > 0 = True
			| otherwise = False
		isSurprise (d:r:_)  
			| r == 0 && (d + 1) == p && d > 0 = True
			| r == 2 && (d + 2) == p = True
			| otherwise = False

divsRems :: [Int] -> [[Int]]
divsRems (x:xs) = [d,r] : divsRems xs
	where
		d = div x 3 :: Int
		r = rem x 3 :: Int
divsRems _ = []


--formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
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
	--print $ cases
	putStr $ unlines output

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]



