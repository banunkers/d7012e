module Main where

main :: IO ()
main = putStr
  (printSubsets (sortSubsets (sizeSubsets (allSubsets [-1, 2, -3, 4, -5]))))

-- Returns all consecutive subsets containing the first int in the list
-- the subsets are returned as a triple with start-, end index and the subset
subsets :: [Int] -> [([Int], Int, Int)]
subsets [x] = [([x], 0, 0)]
subsets (x : xs) =
  ([x], 0, 0) : map (\(tail, i, j) -> (x : tail, i, j + 1)) (subsets xs)

-- Returns a list of all consecutive subsets of a list
-- the subsets are returned as a triple with start-, end index and the subset
allSubsets :: [Int] -> [([Int], Int, Int)]
allSubsets [x] = [([x], 0, 0)]
allSubsets (x : xs) =
  subsets (x : xs) ++ map (\(s, i, j) -> (s, i + 1, j + 1)) (allSubsets xs)

-- Calculates and appends all the subsets sizes
sizeSubsets :: [([Int], Int, Int)] -> [(Int, [Int], Int, Int)]
sizeSubsets [(x, i, j)] = [(sum x, x, i, j)]
sizeSubsets (x : xs   ) = sizeSubsets [x] ++ sizeSubsets xs
 where
  sum :: [Int] -> Int
  sum = foldr (+) 0

-- Sorts the subsets with regard to their sizes from smallest to largest
-- using insertion sort with subset length as decider
sortSubsets :: [(Int, [Int], Int, Int)] -> [(Int, [Int], Int, Int)]
sortSubsets [x     ] = [x]
sortSubsets (x : xs) = ins x (sortSubsets xs)
 where
  ins x [] = [x]
  ins x (h : t)
    | fst' x < fst' h  = x : h : t
    | fst' x == fst' h = if length' x <= length' h then x : h : t else h : x : t
    | otherwise        = h : ins x t
  fst' (x, _, _, _) = x
  length' (_, x, _, _) = length x

-- Returns a list of all the subsets containing (size, list, start-, end index)
getSubsets :: [Int] -> [(Int, [Int], Int, Int)]
getSubsets = sortSubsets . sizeSubsets . allSubsets

formatSubset :: (Int, [Int], Int, Int) -> String
formatSubset (size, subset, i, j) =
  show size ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t" ++ show subset ++ "\n"

formatSubsets :: [(Int, [Int], Int, Int)] -> String
formatSubsets [x     ] = formatSubset x
formatSubsets (x : xs) = formatSubset x ++ formatSubsets xs

printSubsets :: [(Int, [Int], Int, Int)] -> String
printSubsets s = header ++ formatSubsets s
  where header = "size\ti\tj\tsubset\n"

smallestKset :: [Int] -> Int -> IO ()
smallestKset [] _ = error "List is empty, cannot compute smallest k sets"
smallestKset xs k = putStr (printSubsets (take k (getSubsets xs)))
