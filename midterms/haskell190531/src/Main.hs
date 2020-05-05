module Main where
import           Prelude                 hiding ( (.) )

main :: IO ()
main = do
  putStrLn "hello world"

triangle :: Int -> IO ()
triangle 0 = putStr ""
triangle n = do
  putStr ("*" ++ trigRest (n - 1) ++ "\n")
  triangle (n - 1)
 where
  trigRest 0 = ""
  trigRest n = "*" ++ trigRest (n - 1)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

plus n = n + n
mult n = n * n

valid :: [[Int]] -> (Int, Int) -> Bool
valid [] (_, _) = False
valid (x : xs) (i, j) | i == 1    = length x >= j
                      | otherwise = valid xs (i - 1, j)

a :: [[Int]] -> (Int, Int) -> Int
a (x : xs) (i, j) | i == 1 && j == 1 = head x
                  | i == 1           = a (tail x : xs) (i, j - 1)
                  | otherwise        = a xs (i - 1, j)

a2 :: [[Int]] -> (Int, Int) -> Int
a2 m (i, j) = (m !! (i - 1)) !! (j - 1)

escapable :: [[Int]] -> (Int, Int) -> Bool
escapable m (i, j) = checkPath m (i, j) []
 where
  elem2 x []       = False
  elem2 x (a : as) = x == a || elem2 x as
  checkPath :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Bool
  checkPath m (i, j) visited
    | elem2 (i, j) visited
    = False
    | valid m (i, j) && (a m (i, j) == 1)
    = True
    | valid m (i, j) && (a m (i, j) == 0)
    = checkPath m (i + 1, j) (newA (i, j) visited)
      || checkPath m (i - 1, j)     (newA (i, j) visited)
      || checkPath m (i    , j + 1) (newA (i, j) visited)
      || checkPath m (i    , j - 1) (newA (i, j) visited)
    | otherwise
    = False
  newA x l = x : l


m :: [[Int]]
m = -- to do test runs (note: not the example in the exam)
  [ [8, 8, 8, 8, 8, 8, 8, 8, 8, 8]
  , [8, 0, 8, 0, 0, 0, 0, 0, 0, 8]
  , [8, 0, 8, 0, 8, 8, 0, 8, 8, 8]
  , [8, 0, 8, 0, 8, 8, 0, 0, 0, 8]
  , [8, 0, 0, 0, 0, 0, 0, 8, 0, 8]
  , [8, 0, 8, 8, 8, 0, 8, 0, 0, 8]
  , [8, 0, 8, 0, 8, 0, 8, 0, 8, 8]
  , [8, 0, 8, 0, 8, 0, 0, 0, 0, 8]
  , [8, 8, 8, 8, 8, 8, 8, 1, 8, 8]
  ]

compress :: [Int] -> [(Int, Int)]
compress xs = [ (x, y) | x <- xs, y <- xs, abs (x - y) <= 2, x /= y ]

mkList :: Int -> Int -> [Int]
mkList x y = x : mkList (x + diff) (y + diff) where diff = y - x
