module PrjEuler72 where

import Primes (totient)

--length of a farey sequence can be calculated recursively with:

--  f 1 = 2
--  f n = f(n-1)+totient n
f :: Int -> Int
f n = 1 + sum (map totient [1..n])

--must exclude 0/1 and 1/1 from count, so

solution :: Int
solution = f 1000000 - 2

main::IO()
main = do
  print solution