import Data.List

main = print (pairsBelow 10000)

propDivisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n
d n = sum ( propDivisors n )
pairsBelow n = [(i,d i)| i<-[1..n-1], i /= d i, i == d (d i), i < d i]
