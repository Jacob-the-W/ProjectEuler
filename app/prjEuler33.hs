module PrjEuler33 where

import Data.List ( (\\) )

-- Digit cancelling fractions like 49/98 = 4/8 even though crossing 9s isnt valid.

isCancellable :: (Int, Int) -> Bool
isCancellable (a,b) =
  let (a', b') = (show a \\ show b, show b \\ show a)
      (c, d) = (read a', read b')
  in  not (null a') && (d /= 0) && (a*d == b*c) && b /= d && a `mod` 10 /= 0

cancellables :: [(Int, Int)]
cancellables = filter isCancellable [(a,b) | a <- [10..99], b <- [a+1..99]]

solution :: Int  
solution = snd . foldl1 combine $ cancellables where
    combine (a, b) (c, d) = 
      let (a', b', g) = (a*c, b*d, gcd a' b') 
      in (a' `div` g, b' `div` g)

main :: IO ()
main = do
  print cancellables
  print solution