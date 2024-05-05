module PrjEuler67 where

maximumPathSum :: [[Int]] -> Int
maximumPathSum = head . foldr1 (\row prevRow -> 
  zipWith (+) row (zipWith max prevRow (tail prevRow)))

main::IO()
main = do
  input <- readFile "data\\p067_triangle.txt"
  let list = read input::[[Int]]
  print $ maximumPathSum list