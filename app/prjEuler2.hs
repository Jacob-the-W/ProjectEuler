module PrjEuler2 where

fibs :: [Int]
fibs = 1:2:zipWith (+) fibs (tail fibs)

solution :: Int
solution = sum $ takeWhile (<=4*10^6) $ filter even fibs

main :: IO ()
main = do
  print solution