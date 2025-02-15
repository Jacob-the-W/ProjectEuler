module PrjEuler3 (main) where

import Primes (primeFactors)

solution :: Int
solution = maximum . primeFactors $ 600851475143

main :: IO ()
main = print solution