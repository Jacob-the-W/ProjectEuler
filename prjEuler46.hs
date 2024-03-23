-- smallest odd composite that cannot be written as the sum of a prime and twice a square

import Primes (primes,isPrime, composites)
import Data.List ( (\\), group, sort )

oddComposites n = takeWhile (<=n) . filter odd $ composites

sums n = [(n,k,n^2-2*k^2)|k<-[1..ceiling (sqrt (fromIntegral n/2))],isPrime (n-2*k^2)]
solution = minimum $ oddComposites 6000 \\ (map head . group . sort $ map (\(a,b,c) -> a) (concatMap sums $ oddComposites 6000))

main :: IO()
main = do
  print solution