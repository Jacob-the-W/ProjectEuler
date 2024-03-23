import Primes (primes,isPrime)
import Data.List


pandigitalNumbers nDigits  = map (\x ->read x::Integer) $ permutations $ take nDigits "123456789"
pandigitalPrimes nDigits = filter isPrime $ pandigitalNumbers nDigits

solution = [pandigitalPrimes nDigits|nDigits<-[1..9]]

main :: IO()
main = do
  print $ maximum . concat $ solution