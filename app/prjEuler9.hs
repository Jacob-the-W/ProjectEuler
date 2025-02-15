module PrjEuler9 (main) where

solutionsForPerimeter :: Integral a => a -> [a]
solutionsForPerimeter p = [2*k^3*u*v*(u^4-v^4) |
  u <- [1..floor $ (sqrt(1+2*fromIntegral p) - 1) / 2],
  v <- [1..u-1],
  gcd u v == 1, even u == odd v,
  k <- [1..p `div` (2*u*(u+v))],
  2*k*u*(u+v) == p]

main :: IO ()
main = print . head . solutionsForPerimeter $ 1000