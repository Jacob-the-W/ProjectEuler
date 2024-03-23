import Data.List
primes = 2 : filter (\n -> all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..999999]

isPrime x = x `elem` takeWhile (<= x) primes

f n k = sum $ take n $ drop k primes

--while playing around noted that length $ takeWhile (<=1000000) $ scanl1 (+) primes was 546, 
-- so that's an upper bound. just fiddled with k until felt comfortable that increasing our lower bound forces us to shrink the k overall.

result = maximum [(n,k,f n k)|k<-[0..10],n<-[2..546], isPrime (f n k),f n k <=1000000]

main::IO()
main = do
  print result