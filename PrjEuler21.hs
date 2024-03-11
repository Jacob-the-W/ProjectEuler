import Data.List
import Primes (sumPropDivisors)


d = sumPropDivisors
pairsBelow n = [[i,d i]| i<-[1..n-1],d i < n, i < d i, i == d (d i)]
solution = sum $ concat $ pairsBelow 10000

--lets try constructing the abundant numbers first, then feed that into i instead of [1..n-1].

isAbundant n = d n > n

abundants = filter isAbundant [1..]
pairsBelow' n = [[i,d i]| i<-takeWhile (<n) abundants,d i < n, i == d (d i)]


main = do
  print solution
  print $ pairsBelow' $ 10000
