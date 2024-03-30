{-# OPTIONS_GHC -Wno-type-defaults #-}
module PrjEuler35 where
import Primes (primes, isPrime)

-- every prime past 10 must end in a 1, 3, 7, or 9. 
-- 2 and 5 are the only primes that can end in 2 or 5.
potentials :: [Int]
potentials = [2,5] ++ filter (all (`elem` "1379") . show) (takeWhile (<=10^6) primes)

-- without the filter in 'potentials', numbers with '0' in them would break the logic here. 
myCycle :: Int -> Int
myCycle n = let (a,b) = splitAt 1 $ show n in read $ b ++ a

checkLoop :: [Int] -> Bool
checkLoop [] = False
checkLoop as = go [] as where
  go _ [] = False
  go acc (x:xs) | x `elem` acc = True
                | isPrime x = go (x:acc) xs
                | otherwise = False

isPrimeCycle :: Int -> Bool
isPrimeCycle n = checkLoop $ iterate myCycle n

solution::Int
solution = length (filter isPrimeCycle potentials)

main::IO()
main = do
  print solution