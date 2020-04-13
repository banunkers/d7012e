module Lecture1.Exercises where

-- 3.7
threeDifferent2 :: Int -> Int -> Bool
threeDifferent2 m n | m /= n    = True
                    | otherwise = False

threeDifferent3 :: Int -> Int -> Int -> Bool
threeDifferent3 m n p =
  threeDifferent2 m n && threeDifferent2 m p && threeDifferent2 n p

-- 3.8
twoEqual :: Int -> Int -> Bool
twoEqual m n | m == n    = True
             | otherwise = False

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = twoEqual m n && twoEqual m p

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = threeEqual m n p && twoEqual m q

-- 3.17
--numRoots :: Float -> Float -> Float -> Int
--numRoots a b c
--  | b2 > ac4 = 2
--  | b2 == ac4 = 1
--  | otherwise = 0
--  where
--    b2 = b * b
--    ac4 = 4.0 * a * c
--
--lowerRoot :: Float -> Float -> Float -> (Float, Float)
--lowerRoot a b c = 
--
--quadRoot :: Float -> Float -> Float -> ((Float, Float), (Float, Float))
--quadRoot a b c = (lowerRoot a b c, upperRoot a b c)
--  where
--    lowerRoot a b c = 

-- 4.7
natRec :: Int -> Int -> Int
natRec 0 _ = 0
natRec a b = b + natRec (a-1) b

-- 4.8
findSqRoot :: Int -> Int
findSqRoot a = searchSqRoot a a
  where
    searchSqRoot a b
      | b * b <= a = b
      | otherwise = searchSqRoot a (b - 1)

-- 4.9
fMax :: Int -> Int
fMax n = calcFMax n 0 
  where
    calcFMax 0 maxSoFar
      | f 0 > maxSoFar = 0
      | otherwise = maxSoFar
    calcFMax n maxSoFar
      | f n > maxSoFar = calcFMax (n - 1) (f n)
      | otherwise = calcFMax (n - 1) maxSoFar
    
f :: Int -> Int
f 0 = 0
f 1 = 10
f 2 = 17
f 3 = 33
f 4 = 55
f 5 = 10
f 6 = 20
f 7 = 30
f 8 = 99
f _ = 0

raise2 :: Int -> Int
raise2 n
  | n == 0 = 1
  | (n `mod` 2) == 0 = raise2 m * raise2 m
  | otherwise = raise2 m * raise2 m * 2
  where
    m = n `div` 2
