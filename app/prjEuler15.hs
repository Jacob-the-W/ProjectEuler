module PrjEuler15 where

choose :: Integral a => a -> a -> a
choose n k = 
    let a = min k (n-k)
    in product [n - a + 1..n] `div` product [1..a]

-- How many ways can we select 20 right, 20 down from 40?
solution :: Integer
solution = 40 `choose` 20 

main :: IO ()
main = do print solution