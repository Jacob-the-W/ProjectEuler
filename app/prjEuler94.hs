module PrjEuler94 (main) where

import Data.Ratio
import Data.List ( foldl' )

presolution::[Int]
presolution = 
  [2*a+b|
    a<-[5,7..(10^(9 :: Integer)-1) `div` 3],
    b<-[a-1,a+1], 
    let s = (2*a+b)%2
        check = s*(s-b%1) 
        (p,q) = (numerator check, denominator check)
        r = floor . sqrt . fromIntegral $ p,
    q == 1, r^2 == p ]

solution :: Int
solution = foldl' (+) 0 presolution

main :: IO()
main = do
  print presolution
  print solution