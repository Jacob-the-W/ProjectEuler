module PrjEuler5 (main) where

solution :: Int
solution = foldl1 lcm [1..20]

main :: IO ()
main = print solution