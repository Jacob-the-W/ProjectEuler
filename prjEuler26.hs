import Primes (order, primes)
import Data.List

solution = snd . maximum . map (\x->(order 10 x, x)) . takeWhile (<1000) $ primes \\ [2,5]

main = print solution
