module PrjEuler9 where

solutionsForPerimeter :: Integral a => a -> [[a]]
solutionsForPerimeter p = [[a,b,c] |
  u <- [1..floor $ (sqrt(1+2*fromIntegral p) - 1) / 2],
  v <- [1..u-1],
  gcd u v == 1, even u == odd v,
  k <- [1..p `div` (2*u*(u+v))],
  2*k*u*(u+v) == p,
  let (leg1, leg2) = (u^2 - v^2, 2*u*v)
      a = k*min leg1 leg2
      b = k*max leg1 leg2
      c = k*(u^2 + v^2)]

main :: IO()
main = do print $ product $ head $ solutionsForPerimeter 1000