module PrjEuler3 where

import Primes (primeFactors)

solution :: Int
solution = maximum . primeFactors $ 600851475143

main :: IO ()
main = do
  print solution