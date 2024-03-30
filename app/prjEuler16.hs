{-# OPTIONS_GHC -Wno-type-defaults #-}
module PrjEuler16 where

import Data.Char (digitToInt)

solution :: Int
solution = sum $ digitToInt <$> show (2^1000)

main :: IO ()
main = do print solution
