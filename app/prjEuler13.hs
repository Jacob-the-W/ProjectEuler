module PrjEuler13 where

main :: IO()
main = do
  input <- readFile "data\\prjEuler13nums.txt"
  let list = map (\x ->read x::Integer) $ lines input
  let solution = read (take 10 $ show (sum list))::Integer
  print list
  print solution