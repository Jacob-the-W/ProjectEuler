module PrjEuler52 where
--Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
import Data.List ( (\\) )
import Primes (digits)

haveSameDigits :: Integral a => a -> a -> Bool
haveSameDigits x y = null $ digits x \\ digits y

solution :: Int
solution =  head $ filter check [1..] 
  where
    mults x = map (*x) [2..6]
    check x = all (haveSameDigits x) (mults x) 

main :: IO()
main = do
  print solution
  --142857
  --note 1/7 = .142857..