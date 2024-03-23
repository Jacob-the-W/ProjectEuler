import Primes (numOfDistinctPrimeFactors,primes,isPrime)

setup = map (\p -> (p,p+1,p+2,p+3)) [1..]

checking = head $ filter (\(a,b,c,d)->all (\x->numOfDistinctPrimeFactors x==4) [a,b,c,d]) setup
first (a,b,c,d) = a

solution = first checking
main::IO()
main = do
  print solution