module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Assignment 1
goldbach :: Int -> Bool
goldbach n | n <= 2           = error "The input n must be >2"
           | (n `mod` 2) /= 0 = error "The input n is not even"
           | otherwise        = length (primeSumsEqualToN n) > 0
 where
  primeSumsEqualToN n = filter
    (\x -> x == n)
    (map (\(x, y) -> x + y) (zip (primes 2 n) (primes 2 n)))
   where
    primes 2 4 = [2, 3]
    primes 2 6 = [2, 3, 5]
    primes 2 8 = [2, 3, 5, 7]

-- Assignment 2
periodise :: [a] -> [a]
periodise [] = []
periodise xs = xs ++ reverse xs ++ periodise xs
