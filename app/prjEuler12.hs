module PrjEuler12 (main) where

import Primes (numOfDivisors)

triangular :: [Int]
triangular = scanl (+) 0 [1..]

solution :: Int
solution = head $ filter ((>500) . numOfDivisors) triangular

main :: IO ()
main = print solution