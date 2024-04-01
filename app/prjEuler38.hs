module PrjEuler38 where

import Data.List
{--
Since we must have more than one multiple, we can't take the biggest pandigital
 1-9 number and cheat.
So check up to 4 digits since 5000*2 = 10000, i.e., for some 4 digit numbers it
 is possible to concatenate up to a length of 9
--}

isPanDigital :: Int -> Bool
isPanDigital n = sort (show n) == "123456789"

generator :: Int -> Int
generator n 
  | multsConcat = read . take 9 $ concat multiples
  | otherwise = 0 where 
  multiples = show . (n*) <$> [1..9]
  multsConcat = elem 9 . scanl1 (+) $ length <$> multiples

solution :: Int
solution = maximum . filter isPanDigital $ generator <$> [1..9999]

main :: IO()
main = do
  print solution
