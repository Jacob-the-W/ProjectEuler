module PrjEuler32 where

import Data.List ( group, permutations )

panDigitals :: [[Int]]
panDigitals = permutations [1..9]

undigits :: [Int] -> Int
undigits = foldl1 (\x y -> 10*x + y)

test :: [Int] -> Int
test [a,b,c,d,e,f,g,h,i] =
  let first = undigits [a,b] --xx*yyy==zzzz case
      second = undigits [c,d,e]
      rest = undigits [f,g,h,i]
      first' = undigits [a]  --x*yyyy=zzzz case
      second' = undigits [b,c,d,e]
      -- xxyy=zzzzz? 99 *99 = 9801, so impossible. 
  in if first * second == rest || first' * second' == rest then rest else 0
test _ = error "Wrong number of digits for test"

products :: [Int]
products = filter (/=0) $ test <$> panDigitals
uniqueProducts :: [Int]
uniqueProducts = map head . group $ products
solution :: Int
solution = sum uniqueProducts

main :: IO ()
main = do
 print uniqueProducts
 print solution