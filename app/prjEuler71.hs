module PrjEuler71 where

solution :: (Int, Int)
solution = go (2,5) (3,7) 1000000 where
 go (a,b) (c,d) n
   | b+d<=n = go (a+c,b+d) (c,d) n
   | otherwise =  (a,b)

main :: IO ()
main = do
   print solution
   print $ 2+3*((10^6-5) `div` 7) --basically instant
