import Data.List
import Control.Monad
import Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as May

type Case = String
type Solution = String

-- This map is constructed from the cases instructions
-- When running this the first time, it crashed because the "q" -> "z" mapping didn't exist
charMap = Map.fromList (zip bogus correct)
	where
		bogus = "qaozejp mysljylc kd kxveddknmc re jsicpdrysi" ++ "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd" ++ "de kr kd eoya kw aej tysr re ujdr lkgc jv" 
		correct = "zyeqour language is impossible to understand" ++ "there are twenty six factorial possibilities" ++ "so it is okay if you want to just give up"


--solve :: Case -> Solution
solve x = [May.fromJust (Map.lookup c charMap) | c <- x]


formatSolution :: Solution -> String
formatSolution sol = sol


readCase :: IO Case
readCase = do
	line <- getLine
	return line


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
