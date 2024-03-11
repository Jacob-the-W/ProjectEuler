import Data.List
import Primes (isPrime)

result = maximum [[ l, a, b] | 
  a <- [- 999 .. 999], b <- [- 1000 .. 1000], 
  let l = length $ takeWhile isPrime [n ^ 2 + a * n + b | n <- [0 .. ]]]

solution = let [_,a,b] = result in a*b

main::IO()
main = print solution
