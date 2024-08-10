module PrjEuler78 (main) where

import Data.Array ( Array, (!), assocs, listArray )

ps :: Array Int Int
ps = listArray (0,60000) $ map p' [0..60000]
  where f x = if even x then 1 else - 1
        p' 0 = 1
        p' 1 = 1
        p' n = foldl (\x y -> (x+y) `mod` 10^6) 0 [f (k + 1) * (ps ! (n - k * (3 * k - 1) `div` 2)) |k <- ks]
            where ks = takeWhile (\ k -> n - k * (3 * k - 1) `div` 2  >= 0) [1 .. ] ++ takeWhile (\ k -> n - k * (3 * k - 1) `div` 2  >= 0) [- 1, - 2 .. ]

solution :: Int
solution = fst . head $ filter ((0==) . snd) $ assocs ps

main::IO()
main = do
  print solution
