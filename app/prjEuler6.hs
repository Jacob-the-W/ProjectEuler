-- These can be proven by induction or found with generating functions.
sumSquares n = n*(1+2*n)*(1+n) `div` 6
squareSum n = (n*(n+1)`div`2)^2


solution1 n = sum [1..n]^2 - sum [x^2|x<-[1..n]]
-- solution n =  squareSum n - sumSquares n
solution2 n =(n-1)*n*(n+1)*(3*n+2) `div` 12
main::IO()
main = do
    mapM_ (\n->do print (n, solution1 n)) ([(10^)] <*> [0..6])
    mapM_ (\n->do print (logBase 10 (fromIntegral n), logBase 10 . fromIntegral $ solution2 n)) ([(10^)] <*> [0..20])