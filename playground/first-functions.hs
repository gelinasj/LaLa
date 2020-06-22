squareMe v = v * v

fact x = if x == 0 then 1 else x * fact (x - 1)

getCollatzTraceAcc :: Int -> [Int] -> [Int]
getCollatzTraceAcc x acc = if x == 1
				then acc ++ [1]
				else if even x
					then getCollatzTraceAcc (x `div` 2) (acc ++ [x])
					else getCollatzTraceAcc ((3 * x) + 1) (acc ++ [x])

getCollatzTrace x = getCollatzTraceAcc x []

getLongCollatz longLength searchSpace = [n | n <- [1..searchSpace], length (getCollatzTrace n) >= longLength]

sumLists [] = 0
sumLists (lon:rest) = (sum lon) + (sumLists rest)

sumSize llon
	| summ > 100 = "large"
	| otherwise = "small"
	where extra = 3
	      summ = sumLists llon + extra
	      7 = summ

makeInfiniteListOf :: a -> [a]
makeInfiniteListOf item = map (\_ -> item) [0..]


transpose :: [[a]] -> [[a]]
transpose matrix = foldr transposeFold (makeInfiniteListOf []) matrix

transposeFold :: [a] -> [[a]] -> [[a]]
transposeFold row acc = zipWith (:) row acc
