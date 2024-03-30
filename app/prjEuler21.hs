module PrjEuler21 where

import Primes (amicablePairs)

solution :: Int 
solution = sum . concat . takeWhile (not . null) . map (\(a,b)->filter (<=10000) [a,b]) $ amicablePairs

main :: IO ()
main = print solution