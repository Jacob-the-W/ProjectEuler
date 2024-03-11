import Data.List

import Primes (sumPropDivisors)

isAbundant :: Integer -> Bool
isAbundant n = (n /= 1) && (sumPropDivisors n > n)

abundantList :: [Integer]
abundantList = [x | x <- [12..28123], isAbundant x]

isSumOfAbundants :: Integer -> Bool
isSumOfAbundants n = any (\x -> isAbundant (n-x)) (takeWhile (<=n) abundantList)

sumNotSumOfAbundants :: Integer -> Integer
sumNotSumOfAbundants n = sum [x | x <- [1..n], not (isSumOfAbundants x)]

-- Call the sumNotSumOfAbundants function with a limit of 28123
--I cheat by finding the maximum first
main::IO()
main = do
  print $ sumNotSumOfAbundants 20161
