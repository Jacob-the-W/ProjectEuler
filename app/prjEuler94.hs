module PrjEuler94 (main) where

import Data.List ( foldl' )

-- Call the same sides a, the different side b
-- Heron's formula gives us 
-- A = sqrt (s(s-a)(s-a)(s-b))
--   = (s-a) sqrt (s(s-b))
-- which is an integer iff s(s-b) is a perfect square
-- s(s-b) = (2a+b)/2*(2a-b)/2 is an integer and a perfect square
-- 4a^2 - b^2 must be multiple of 4 and perfect square
-- b^2 must be multiple of 4, so b must be even, a must be odd.
presolution::[Int]
presolution =
  [2*a+b|
    a<-[5,7..(10^9-1) `div` 3],
    b<-[a-1,a+1],
    let check = 4*a^2-b^2
        r = floor . sqrt . fromIntegral $ check,
    r^2 == check ]

solution :: Int
solution = foldl' (+) 0 presolution

main :: IO()
main = do
  print presolution
  print solution
