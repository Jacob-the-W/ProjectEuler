module PrjEuler24 (main) where
    
import Data.List ( (\\) )

-- Instead of importing permutations from Data.List, define it ourselves
-- If the input is sorted, the output will be sorted, unlike the default.
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x<-xs, ys <- permutations (xs \\ [x])]

solution :: String
solution = permutations "0123456789" !! 999999

main :: IO ()
main = putStrLn solution