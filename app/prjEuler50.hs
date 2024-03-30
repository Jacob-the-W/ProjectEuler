
import Primes (primes')

isPrime :: Int -> Bool
isPrime n = 
  let r = floor . sqrt $ fromIntegral n
  in all (\p -> n `rem` p /=0) . takeWhile (<=r) $ primes'

f :: Int -> Int -> Int
f n k = sum . take n . drop k $ primes'

--while playing around noted that length $ takeWhile (<=1000000) $ scanl1 (+) primes was 546, 
-- so that's an upper bound. just fiddled with k until felt comfortable that increasing our lower bound forces us to shrink the k overall.

result :: (Int, Int, Int)
result = maximum [(n,k,c)|k<-[0..10],n<-[2..546], let c = f n k, isPrime c, c <=1000000]

main::IO()
main = do
  print result
