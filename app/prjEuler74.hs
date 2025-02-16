module PrjEuler74 (main) where

import Data.List ( group, sort )

import Primes (digits)
import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

fac :: Int -> Int
fac n = product [1..n]

sumOfFacOfDigits :: Int -> Int
sumOfFacOfDigits = sum . map fac . digits 

takeUntilCycle :: [Int] -> IntSet
takeUntilCycle = go Set.empty where
  go acc [] = acc
  go acc (x:xs)
    | x `Set.member` acc = acc
    | otherwise =  go (Set.insert x acc) xs

loop :: Int -> IntSet
loop = takeUntilCycle . iterate sumOfFacOfDigits

solution :: Int
solution = 
  sum 
    [ count
      | n<-[1..10^6-1]
      , let d = show n
            d' = sort d
      , d' == d || elem '0' d
      , Set.size (loop n) == 60
      , let count = if '0' `elem` d then 1 else multinomial
            multinomial = 
              let bottom = product . map (fac . length) . group $ d'
                  top = fac (length d)
              in top `div` bottom
    ]
--10^6-1 = 999999

--better way to count with permutations, but the 0! not being able to be a lead digit is throwing me off

main :: IO ()
main = print solution

--head $ dropWhile (all(==1) occurrences) $ 