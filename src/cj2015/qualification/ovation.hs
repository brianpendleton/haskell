import Data.List
import Control.Monad
import Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as May
import Data.Char

type Case = [String]
type Solution = Int

solve :: Case -> Solution
solve (x:lst:_) = snd adds
	where
		maxShy = read x :: Int 
		audience = (map digitToInt lst)
		pairs = zip audience [0..]
		adds = foldl countAdds (0, 0) pairs

countAdds :: (Int, Int) -> (Int, Int) -> (Int, Int)
countAdds (standing, added) (num, shyness)
	| num > 0 && standing <= shyness = (audienceTotal + addsNeeded, added + addsNeeded)
	| otherwise = (audienceTotal, added)
	where
		addsNeeded = shyness-standing
		audienceTotal = standing + num

formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	line <- getLine
	return (words line)


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
