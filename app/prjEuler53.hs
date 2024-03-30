module PrjEuler53 where

choose :: Integer -> Integer -> Integer
choose n k =
  if k < 0 || k > n then 0 else
  let a = min k (n-k)
  in product [n - a + 1..n] `div` product [1..a]

solution :: Int
solution = length $ [(n,r) | n<-[23..100],r<-[0..n],choose n r > 1000000]

main :: IO()
main = do print solution