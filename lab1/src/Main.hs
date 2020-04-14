module Main where

main :: IO ()
main = testCase1

-- Returns all consecutive subsets containing the first int in the list
-- the subsets are returned as a triple with (subset, start-, end index)
subsets :: [Int] -> [([Int], Int, Int)]
subsets [x] = [([x], 1, 1)]
subsets (x : xs) =
  ([x], 1, 1) : map (\(tail, i, j) -> (x : tail, i, j + 1)) (subsets xs)

-- Returns a list of all consecutive subsets of a list
allSubsets :: [Int] -> [([Int], Int, Int)]
allSubsets [x] = [([x], 1, 1)]
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
-- using insertion sort
sortSubsets :: [(Int, [Int], Int, Int)] -> [(Int, [Int], Int, Int)]
sortSubsets [x     ] = [x]
sortSubsets (x : xs) = ins x (sortSubsets xs)
 where
  ins x [] = [x]
  ins x (h : t) | fst' x <= fst' h = x : h : t
                | otherwise        = h : ins x t
  fst' (x, _, _, _) = x

-- Returns a sorted list of all the subsets 
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

-- Test cases
testCase1 = smallestKset [ x * (-1) ^ x | x <- [1 .. 100] ] 15
testCase2 = smallestKset [24, -11, -34, 42, -24, 7, -19, 21] 6
testCase3 =
  smallestKset [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3] 8
