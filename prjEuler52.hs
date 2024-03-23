--Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
import Data.List ( (\\) )

haveSameDigits x y = null $ show x \\ show y

solution :: Int
solution =  head $ filter check [1..] where
  mults x = map (*x) [2..6]
  check x = all (haveSameDigits x) (mults x) 

main :: IO()
main = do
  print solution
  --142857
  --note 1/7 = .142857..