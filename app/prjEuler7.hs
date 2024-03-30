module PrjEuler7 where
import Primes (primes)

solution :: Int
solution = primes !! 10000

main :: IO ()
main = do print solution