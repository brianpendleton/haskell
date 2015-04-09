import qualified Data.Set as S
import Data.List
import Control.Monad

type Case = (Int, [String])
type Solution = Int


solve :: Case -> Solution
solve (n, xs) = count
	where (_,count,_) = foldl countSwitches (n, 0, S.empty) xs


countSwitches (numEng, numSwitch, set) x
		| needsSwitch = (numEng, numSwitch+1, new_set)
		| otherwise = (numEng, numSwitch, pool)
		where
			needsSwitch = S.size pool == numEng
			pool = S.insert x set
			new_set = S.insert x S.empty


formatSolution :: Solution -> String
formatSolution sol = show sol


readCase :: IO Case
readCase = do
	n <- getLine
	engs <- replicateM (read n) getLine
	s <- getLine
	searches <- replicateM (read s) getLine
	return (read n, searches)


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

