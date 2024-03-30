module PrjEuler10 where

import Primes (primesToUA)
import Data.List (foldl')

solution :: Int
solution = foldl' (+) 0 $ primesToUA 2000000

main :: IO ()
main = do print solution