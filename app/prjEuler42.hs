module PrjEuler42 where

import Data.Char ( ord )

scoreLetter :: String -> [Int]
scoreLetter = map (\x -> ord x - 64)

triangle :: [Int]
triangle = scanl1 (+) [1..]

triangular :: Int -> Bool
triangular s = s `elem` takeWhile (<=s) triangle

main::IO()
main = do
  list <- readFile "data\\p042_words.txt"
  let readList = read ("["++list++"]")
  let scores = map (sum . scoreLetter) readList
  putStrLn "Here's the list with all the scores for each name."
  print $ zip readList scores
  putStrLn "And here's the scores which are triangular numbers:"
  print $ filter triangular scores
  putStrLn "Length of that list"
  print $ length $ filter triangular scores