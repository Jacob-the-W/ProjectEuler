module PrjEuler20 (main) where

import Data.Char ( digitToInt )

solution :: Int
solution = sum $ digitToInt <$> show (product [1..100])

main :: IO ()
main = print solution