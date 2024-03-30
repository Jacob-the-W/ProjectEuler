module PrjEuler45 where

pent :: [Int]
pent = map (\n->n*(3*n-1) `div` 2) [166..]

hex :: [Int]
hex = map (\n->n*(2*n-1)) [143..]

-- n*(2n-1) = p
-- 2n^2 - n - p = 0
-- n = (1 + sqrt(1+8*p))/4
-- is n an integer?
-- 1 + sqrt (1 + 8*p) must be = 0 (mod 4)
-- sqrt (1 + 8*p) must be = 3 (mod 4)
-- 1 + 8*p must be a perfect square, = 1 (mod 4)
-- 8*p + 1 = 1 (mod 4) for free
isHex :: Int -> Bool
isHex p = 
  let rad = 1+8*p 
      r = floor . sqrt . fromIntegral $ rad
  in r^2 == rad

result :: Int
result = head. drop 1 $
         filter isHex pent

main::IO()
main = do
  print result

--not great 
--filter (\t -> is `elem` (takeWhile (<= t) pent) [t|t<-(drop 284 tri)
-- filter (\t -> t `elem` (takeWhile (<= t) tri)) [t|t<-hex]