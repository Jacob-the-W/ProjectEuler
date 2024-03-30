module PrjEuler41 where

import Primes (isPrime)
import Data.List ( permutations )

pandigitalNumbers :: Int -> [Int]
pandigitalNumbers nDigits  = read <$> (permutations . take nDigits $ "123456789")

pandigitalPrimes :: Int -> [Int]
pandigitalPrimes = filter isPrime . pandigitalNumbers

solution :: Int
solution = maximum $ pandigitalPrimes =<< [1..9]

main :: IO()
main = do
  print solution