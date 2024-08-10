module PrjEuler68 where

import Data.List ( (\\) )
    
list :: Int -> [[Int]]
list n = [[a,b,c,d,c,e,g,e,f,i,f,h,j,h,b]|
  a <- [1..10], b <- [1..10] \\ [a], 
  let c = n - a - b, c > 0, c <= 10, c `notElem` [a,b], 
  d <- [1..10] \\ [a,b,c], 
  let e = n - c - d, e > 0, e <= 10, e `notElem` [a,b,c,d],
  f <- [1..10] \\ [a,b,c,d,e],
  let g = n - e - f, g > 0, g <= 10, g `notElem` [a,b,c,d,e,f], 
  h <- [1..10] \\ [a,b,c,d,e,f,g], 
  let i = n - h - f, i > 0, i <= 10, i `notElem` [a,b,c,d,e,f,g,h],
  let j = n - h - b, j > 0, j <= 10, j `notElem` [a,b,c,d,e,f,g,h,i],
  all (a <) [d,g,i,j]]

solution :: String
solution = maximum . filter ((==16) . length) $ (map (show =<<) . list) =<< [13..18]

main :: IO ()
main = do putStrLn solution