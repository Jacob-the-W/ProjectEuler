module PrjEuler7 (main) where
import Primes (primes)

solution :: Int
solution = primes !! 10000

main :: IO ()
main = print solution