module PrjEuler30 where

import Data.List
import Data.Char

-- 999999  -> 6*9^5 = 354294, the biggest 6 digit number can only get mapped up to a 6 digit number
-- 9999999 -> 7*9^5 = 413343, the biggest 7 digit number can only get mapped up to a 6 digit number 
input :: [Int]
input = [11..354294]

fix :: Int -> Int
fix = sum . map ((^5) . digitToInt) . show

fixed :: [Int]
fixed = filter (\x -> x == fix x) input

solution :: Int
solution = foldl' (+) 0 fixed




main::IO()
main = do 
  print fixed
  print solution