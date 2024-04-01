module PrjEuler42 where

import Data.Char ( ord )

scoreLetter :: String -> [Int]
scoreLetter = map (\x -> ord x - 64)

scoreWord :: String -> Int
scoreWord = sum . scoreLetter

triangles :: [Int]
triangles = map (\n -> n*(n+1) `div` 2) [0..]

-- | Invert a triangular number, noticed this after doing 45
-- n*(n+1)/2 = t
-- n^2 + n - 2t = 0
-- n = (-1 + sqrt(1+8t))/2 must be an integer, 
-- so -1 + sqrt (1 +8t) must be even,
-- sqrt ( 1 + 8t) must be odd
-- (1 + 8t) must be odd, which we get for free
-- Make sure 1+8t is a perfect square.
triangular :: Int -> Bool
triangular s = 
  let rad = 1 + 8*s
      r = floor . sqrt . fromIntegral $ rad
  in r*r == rad

main :: IO ()
main = do
  input <- readFile "data\\p042_words.txt"
  let list = read $ "[" ++ input ++ "]"
      scores = filter triangular $ scoreWord <$> list
      solution = length scores
  print solution