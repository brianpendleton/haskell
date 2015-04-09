import qualified Data.Set as S
import Data.List

type Case = (Int, [String])

readCase :: [String] -> Case
readCase (n:ts) = let
		(x:xs) = drop (read n) ts 
		searches = take (read x) xs
	in (read n, searches)

readCases :: String -> [Case]
readCases x = 
	let (n:ts) = lines x 
	in 	take (read n) $ map readCase ts

main = do
	ts <- readCases `fmap` getContents
	flip mapM_ (zip [1..] ts) $ \(i, (n, sch)) -> do
		putStr $ "Case #" ++ show i ++ ": "
		print $ getSearches' (n, sch)

--main = interact $ show . getSearches . tail . lines

countSwitches (numEng, numSwitch, set) x = 
	if S.size pool == numEng then (numEng, numSwitch+1, S.empty) else (numEng, numSwitch, set)
	where pool = S.insert x set

getSearches :: [String] -> Int
getSearches input =   
    let (n:ts) = input
    	(x:xs) = drop (read n) ts
        res = take (read x) xs
        (_, c, _) = foldl countSwitches (read n, 0, S.empty) res
    in  c

getSearches' :: Case -> Int
getSearches' (n, xs) = let (_, c, _) = foldl countSwitches (n, 0, S.empty) xs in c
