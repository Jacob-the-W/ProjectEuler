module Main (main, ws) where

ws :: Int -> [Int] -> Int
ws 0 _ = 1
ws _ [] = 0
ws n (c:cs) = sum [ws (n - x*c) cs | x <- [0..n `div` c]]

solution :: Int
solution = ws 200 [200,100,50,20,10,5,2,1]

main = do
  print solution
