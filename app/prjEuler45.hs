module PrjEuler45 where

tri :: [Int]
tri = map (\n->n*(n+1) `div` 2) [286..]

pent :: [Int]
pent = map (\n->n*(3*n-1) `div` 2) [166..]

hex :: [Int]
hex = map (\n->n*(2*n-1)) [144..]

-- ** Inverting triangular numbers:
-- 
-- n*(n+1)/2 = t
-- n^2 + n - 2t = 0
-- n = (-1 + sqrt (1+8*t))/2
-- is n an integer?
-- 1 + 8*t must be a perfect square,
-- -1 + sqrt(1+8*t) must be even
-- sqrt(1+8*t) must be odd
-- 1+8*t must be odd, which we get for free
-- r^2 = 1 (mod 2) => r = 1 (mod 2), so we don't need to bother checking.
-- This means that we can just check if the square root is an integer.

-- ** Inverting pentagonal numbers:
--
-- n*(3n-1)/2 = t
-- 3n^2 - n - 2t = 0
-- n = (1+ sqrt (1 + 24*t))/6
-- is n an integer?
--  1 + 24*t must be a perfect square,
--  1 + sqrt(1+24*t) must be 0 (mod 6)
--  sqrt(1+24*t) must be -1 (mod 6)
--  1 + 24*t must be a perfect square, = 1 (mod 6), which we get for free
--  r^2 = 1 (mod 6) => r = 1 (mod 6) or r = 5 (mod 6), so we have to check.

-- ** Inverting hexagonal numbers:
-- 
-- n*(2n-1) = t
-- 2n^2 - n - t = 0
-- n = (1 + sqrt (1+8*t))/4
-- is n an integer?
--  1 + sqrt (1 + 8*t) must be 0 (mod 4)
--  sqrt (1 + 8*t) must be -1 (mod 4)
--  1 + 8*t must be a perfect square, = 1 (mod 4), which we get for free
--  r^2 = 1 (mod 4)  => r = 1 (mod 4) or r = 3 (mod 4), so we have to check.
--
-- This doubles as a proof that the hexagonal numbers are a subset of the 
-- triangular numbers, i.e., every hexagonal number is triangular as well.
-- Instead of checking if a triangular number is hexagonal and pentagonal,
-- we can check if a pentagonal number is hexagonal, which is guaranteed
-- to be triangular.
isHex :: Int -> Bool
isHex t = 
  let rad = 1+8*t 
      r = floor . sqrt . fromIntegral $ rad
  in r^2 == rad && r `mod` 4 == 3

result :: Int
result = head . filter isHex $ pent

main::IO()
main = do print result