module PrjEuler74 where

import Data.List

import Primes (digits)

fac :: Int -> Int
fac n = product [1..n]

sumOfFacOfDigits :: Int -> Int
sumOfFacOfDigits n = foldl' (+) 0 (map fac $ digits n)

occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (== x)

takeUntilCycle :: Eq a => [a] -> [a]
takeUntilCycle = go [] where
  go acc [] = reverse acc
  go acc (x:xs)
    | x `elem` acc = acc
    | otherwise =  go (x:acc) xs

loop :: Int -> [Int]
loop n = takeUntilCycle $ iterate sumOfFacOfDigits n

solution :: Int
solution =
  sum [count n|n<-[1..10^6-1],
    let d = show n,
    sort d == d || elem '0' d,
    length (loop n) == 60]
  where
  count n = if '0' `elem` show n then 1 else multinomial n
  multinomial n =
    let d = show n
        bottom = product . map (fac . length) . group . sort $ d
        top = fac (length d)
    in top `div` bottom
--10^6-1 = 999999

--better way to count with permutations, but the 0! not being able to be a lead digit is throwing me off

main :: IO()
main = do
  print solution

--head $ dropWhile (all(==1) occurrences) $ 