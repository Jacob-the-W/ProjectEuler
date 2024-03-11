main = print (pairsBelow 10000)

propDivisors n = [m|m<-[1..(n-1)], mod n m==0 ]
d n = sum ( propDivisors n )
pairsBelow n = [(i,d i)| i<-[1..n-1], i /= d i, i == d (d i), i < d i]