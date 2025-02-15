module PrjEuler94 (main) where

-- Instead of Heron's formula, generate pythagorean tuples [a,b,c]
-- such that c / a ~= 2, and consider the triangle with side lengths
-- [c, c, 2*a] which are almost equilateral.
-- perimeter at most c + c + 2a = c + c + (c + 1) <= 10^9
-- m^2 + n^2 = c <= (10^9-1) / 3
presolution :: [Int]
presolution = 
  [2*(a+c)|
    m <- [1..floor $ sqrt(2*10^9-5)/(2*sqrt 3) - 1/2] -- m^2 + (m+1)^2 <= (10^9 - 1) / 3
    , n <-[1..min (m-1) (floor . sqrt . fromIntegral $ (10^9 - 1) `div` 3 - m^2)]
    , let c = m^2 + n^2
    , c `mod` 4 == 1 -- even m == odd n
    , let a = min (m^2-n^2) (2*m*n)
    , m^2 + n^2  <= (10^9 - 1) `div` 3
    , abs (2*a - c ) == 1
    , gcd m n == 1]

solution :: Int
solution = sum presolution

main :: IO ()
main = print solution
