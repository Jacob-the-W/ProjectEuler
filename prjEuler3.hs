import Primes (primeFactors)

solution = maximum $ primeFactors 600851475143

main :: IO ()
main = do
  print solution