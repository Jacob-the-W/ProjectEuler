module PrjEuler12 where

import Primes (numOfDivisors)

triangular :: [Int]
triangular = map (\n->n*(n+1) `div` 2) [1..]
solution :: Int
solution = head $ filter (\n -> numOfDivisors n > 500) triangular

main :: IO ()
main = do 
  print solution