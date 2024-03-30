{-# OPTIONS_GHC -Wno-type-defaults #-}
module PrjEuler35 where
import Primes (primes, isPrime)

potentials :: [Int]
potentials = [2,5] ++ filter (all (`elem` "1379") . show) (takeWhile (<=10^6) primes)
-- every prime past 10 must end in a 1, 3, 7, or 9. Cycling any other digit breaks it.

myCycle :: Int -> Int
myCycle n = 
  let digits = floor $ logBase 10 (fromIntegral n) -- technically digits + 1
      (q, r) = n `quotRem` (10^digits)
  in 10*r + q

isPrimeCycle :: Int -> Bool
isPrimeCycle n = 
  let (i:is) = take 6 . iterate myCycle $ n
      cycles = i:takeWhile isPrime is
  in length cycles == 6


solution::Int
solution = length (filter isPrimeCycle potentials)

main::IO()
main = do
  print solution