module Lecture2.Exercises where

-- 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) | a > b && a > c && b > c = (a, b, c)
                      | a > b && a < c && b < c = (c, a, b)
                      | a > b && a > c && b < c = (a, c, b)
-- 5.10
divisors :: Int -> [Int]
divisors n = [ x | x <- take n nat, n `mod` x == 0 ]
  where nat = 1 : [ x + 1 | x <- nat ]

isPrime :: Int -> Bool
isPrime n | divisors n == [1, n] = True
          | otherwise            = False

-- 5.11
matches :: Int -> [Int] -> [Int]
matches k xs = [ x | x <- xs, x == k ]

elemInList :: Int -> [Int] -> Bool
elemInList k xs | not (null (matches k xs)) = True
                | otherwise                 = False

-- 5.22
onSeperateLines :: [String] -> String
onSeperateLines xs = concatMap (++ "\n") xs

-- 5.23
duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate n k = n ++ duplicate n (k - 1)

-- 5.24
pushRight :: Int -> String -> String
pushRight l s = duplicate " " l ++ s

-- 7.2
addFirst2 :: [Int] -> Int
addFirst2 []       = 0
addFirst2 [x     ] = x
addFirst2 (x : xs) = x + head xs

-- 7.4
prod :: [Int] -> Int
prod []       = 1
prod (x : xs) = x * head xs * prod (tail xs)

-- 7.4 foldr
prod2 :: [Int] -> Int
prod2 x = foldr (*) 1 x

-- 7.5
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 7.7
unique :: [Int] -> [Int]
unique [] = []
unique (x : xs) 
  | isUnique x xs = x : unique xs
                | otherwise     = unique (filter (x /=) xs)
  where isUnique x xs = filter (x /=) xs == xs

-- 7.8
rev :: [t] -> [t]
rev []       = []
rev [x     ] = [x]
rev (x : xs) = rev xs ++ [x]

unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (map fst xs, map snd xs)

-- 7.14
drop' :: Int -> [a] -> [a]
drop' 0 xs               = xs
drop' _ []               = []
drop' n (x : xs) | n > 0 = drop (n - 1) xs
