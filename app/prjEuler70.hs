module PrjEuler70 (main) where

import Primes (totient, sieveUA)
import Data.List ( (\\) )
import Data.Array.Unboxed ( assocs )

compositesTo :: Int -> [Int]
compositesTo n = [2*i+1 |(i,False) <- assocs . sieveUA $ n]

isPermutation :: Int -> Int -> Bool
isPermutation x x' = 
  let (s, s') = (show x, show x') 
  in null (s \\ s')

solution :: Int
solution = go 21 12 $ compositesTo (10^7 - 1) -- totient 21 = 12
  where
    go m _ [] = m
    go m tm (a:as)
      | m*ta > a*tm && isPermutation a ta = go a ta as -- m/tm > a/ta
      | otherwise = go m tm as
      where ta = totient a 
      

main :: IO ()
main = print solution
