module PrjEuler10 (main) where

import Primes (primesToUA)

solution :: Int
solution = sum $ primesToUA 2000000

main :: IO ()
main = print solution