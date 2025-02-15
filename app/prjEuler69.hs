module PrjEuler69 (main) where

import Primes (totient)

solution :: Integer
solution = snd . maximum . map (\n ->(fromIntegral n/fromIntegral (totient n),n)) $ [1..10^6]


main :: IO ()
main = print solution
