module PrjEuler5 where

solution :: Int
solution = foldl1 lcm [1..20]

main :: IO()
main = do print solution