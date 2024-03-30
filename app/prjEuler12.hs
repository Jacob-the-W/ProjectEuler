import Primes (numOfDivisors)

triangular = map (\n->n*(n+1) `div` 2) [1..]
solution = head $ filter (\n -> numOfDivisors n > 500) triangular

main :: IO ()
main = do 
  print solution