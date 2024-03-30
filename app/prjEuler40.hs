module PrjEuler40 where
  
import Data.Char

list :: [Int]
list = digitToInt <$> (show =<< [1..])

d :: Int -> Int
d n = list !! (n-1)

solution :: Int
solution = product $ d . (10^) <$> [0..6]

main :: IO()
main = do
  print solution