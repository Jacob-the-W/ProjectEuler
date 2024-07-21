--bending wires into right triangles, L= a + b + c, a^2+b^2==c^2., 
--Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one integer sided right angle triangle be formed?

-- (a,b,c) = map (k*) (u^2-v^2,2*u*v,u^2+v^2), satisfies Pythag Thm. by construction.

--Perimeter = k(2*u^2+2*u*v) = 2*k*u*(u+v), need perimeter less than<= 1.5million, so for small k and since u>v, 2*k*u*(u+v)>2*k*v*(v+v) >= 2v*(2v) = 4v^2 so, v ~ 612.37
import Data.List
pythagoreans n =
  [map (k *) [u ^ 2 - v ^ 2, 2 * u * v, u ^ 2 + v ^ 2] |
     u <- [1 .. ceiling (sqrt (fromIntegral n) / 4)],
     v <- [1 .. u - 1],
     gcd u v == 1,
     mod u 2 /= mod v 2,
     k <- takeWhile (\ k -> 2 * k * u * (u + v) <= n) [1 .. ]]

test n =
  [k * 2 * u * (u + v) |
     u <- [1 .. ceiling (sqrt (fromIntegral n))],
     v <- [1 .. u - 1],
     gcd u v == 1,
     mod u 2 /= mod v 2,
     k <- takeWhile (\ k -> 2 * k * u * (u + v) <= n) [1 .. ]]

solution = length $ filter (==1) $ map length $ group $ sort $ test 1500000


main::IO()
main = do
  print solution