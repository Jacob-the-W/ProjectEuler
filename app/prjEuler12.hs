module PrjEuler12 where

import Primes (numOfDivisors)

triangular :: [Int]
triangular = map (\n->n*(n+1) `div` 2) [1..]

solution :: Int
solution = head $ filter ((>500) . numOfDivisors) triangular

main :: IO ()
main = do 
  print solution