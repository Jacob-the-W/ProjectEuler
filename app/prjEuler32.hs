module PrjEuler32 where

import Data.List ( group, permutations )

panDigitals :: [String]
panDigitals = permutations "123456789"

test :: [Char] -> Int
test [a,b,c,d,e,f,g,h,i] = 
  let first = read [a,b]::Int --xx*yyy==zzzz case
      second = read [c,d,e]::Int
      rest = read [f,g,h,i]::Int    
      first' = read [a]::Int  --x*yyyy=zzzz case
      second' = read [b,c,d,e]::Int
      -- xxyy=zzzzz? 99 *99 = 9801, so impossible. 
  in if first * second == rest || first' * second' == rest then rest else 0
  
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