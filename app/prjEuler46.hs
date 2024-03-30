-- smallest odd composite that cannot be written as 
-- the sum of a prime and twice a square

import Primes (sieveUA)
import Data.Array.Unboxed (assocs, (!), UArray)

arr :: UArray Int Bool
arr = sieveUA 6000 

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = (n>2) && odd n && (arr ! (n `div` 2))

oddComposites :: [Int]
oddComposites = [2*i + 1 | (i, False) <- assocs arr]

check :: Int -> Bool
check n = not $ any (\k -> isPrime (n - 2*k^2)) [1..floor (sqrt (fromIntegral n / 2))]

solution :: Int
solution = head . filter check $ oddComposites

main :: IO()
main = do
  print solution
