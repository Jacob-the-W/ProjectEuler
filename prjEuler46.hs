-- smallest odd composite that cannot be written as the sum of a prime and twice a square

import Primes (isPrime, composites)

oddComposites = filter odd composites

check n = not $ any (\k -> isPrime (n - 2*k^2)) [1..ceiling (sqrt (fromIntegral n))]
solution = head . filter check $ oddComposites

main :: IO()
main = do
  print solution
