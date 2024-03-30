module PrjEuler27 where

import Primes (isPrime)

result :: (Int, Int, Int)
result = maximum [( l, a, b) | 
  a <- [- 999 .. 999], b <- [- 1000 .. 1000], 
  let l = length $ takeWhile isPrime [n ^ 2 + a * n + b | n <- [0 .. ]]]

solution :: Int
solution = let (_,a,b) = result in a*b

main::IO()
main = print solution
