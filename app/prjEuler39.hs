module PrjEuler39 where

import Data.Function (on)
import Data.List (maximumBy)

-- just counting these, so don't need 
-- Int -> [(Int, Int, Int)]
solutionsForPerimeter :: Int -> [()] 
solutionsForPerimeter p = [() |
  u <- [1..floor $ (sqrt(1+2*fromIntegral p) - 1) / 2],
  v <- [1..u-1],
  gcd u v == 1, even u == odd v,
  k <- [1..p `div` (2*u*(u+v))],
  2*k*u*(u+v) == p]

lengthOfSolutions :: Int -> Int
lengthOfSolutions = length . solutionsForPerimeter 

solution :: Int
solution = maximumBy (compare `on` lengthOfSolutions) [12..1000]

main::IO()
main = do
  print solution