module PrjEuler75 where

import Data.List

perimeters :: Int -> [Int]
perimeters n =
  [k * 2 * u * (u + v) |
     -- 2u(u+1) <= n
     -- u <= (sqrt (2n+1) - 1)/2
     u <- [1 .. floor $ (sqrt (fromIntegral $ 2*n + 1) - 1) / 2], 
     v <- [1 .. u - 1],
     gcd u v == 1,
     mod u 2 /= mod v 2,
     k <- [1 .. floor $ fromIntegral n/fromIntegral (2*u*(u+v))]]

solution :: Int
solution = length . filter (null . tail) . group . sort $ perimeters 1500000


main::IO()
main = do
  print solution