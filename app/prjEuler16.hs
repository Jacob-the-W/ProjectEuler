module PrjEuler16 (main) where

import Data.Char (digitToInt)

solution :: Int
solution = sum $ digitToInt <$> show (2^1000)

main :: IO ()
main = print solution
