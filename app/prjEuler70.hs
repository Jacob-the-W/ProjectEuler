module PrjEuler70 where

import Primes (totient, sieveUA)
import Data.List
import Data.Array.Unboxed

compositesTo :: Int -> [Int]
compositesTo n = [2*i+1 |(i,False) <- assocs . sieveUA $ n]

isPermutation :: Int -> Int -> Bool
isPermutation x x' = 
  let (s, s') = (show x, show x') 
  in null (s \\ s')

ratio :: Int -> Float
ratio n = fromIntegral n/fromIntegral (totient n)

solution :: Int
solution = snd . minimum . map (\n -> (ratio n,n)) . filter (\n -> isPermutation n $ totient n) $ compositesTo (10^7 - 1)

main :: IO()
main = do
  print solution
