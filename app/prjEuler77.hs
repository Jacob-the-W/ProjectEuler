module PrjEuler77 (main) where

import Primes (primeFactors)
import Data.List ( nub )

pps :: Int -> Int
pps = (map pps' [0..]!!)
  where pps' 0 = 0
        pps' 1 = 0
        pps' 2 = 1
        pps' n = (sum (nub (primeFactors n))+sum [sum (nub (primeFactors j))*pps (fromIntegral (n-j))|j<-[1..n-1]]) `div` n

solution :: Int
solution = head $ dropWhile (\x-> pps x<5000) [2..]

main::IO()
main = do
  print solution