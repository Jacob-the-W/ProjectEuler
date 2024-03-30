module PrjEuler43 where

import Data.List (delete, tails)

undigits :: [Int] -> Int
undigits = foldl1 (\x y -> 10*x + y)

-- Instead of using Data.List.permutations, make our own
-- Can't start with 0
permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [x:ys | x<-xs, x /= 0, ys<-permutations' (delete x xs)]
  where permutations' [] = [[]]
        -- 0 is allowed in the rest of the list
        permutations' xs_ = [x:ys | x<-xs_, ys<-permutations' (delete x xs_)]

primes :: [Int]
primes = [2,3,5,7,11,13,17]

panDigitals :: [[Int]]
panDigitals = permutations [0..9]

solution :: Int
solution = sum $ undigits <$> filter tests panDigitals where
  tests [] = True
  tests xs = and $ zipWith test primes (drop 1 . take 8 $ tails xs)
  test p n = chunk `mod` p == 0 where chunk = undigits (take 3 n)



main :: IO ()
main = do print solution