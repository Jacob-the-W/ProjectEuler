module PrjEuler76 where

--since we merely count, instead use recurrence relation
import Primes (sumDivisors)

--p 0 = 1
--p n =(sum [sumDivisors (n-k)*p(k)|k<-[0..n-1]]) `div` n
--slow, but candidate for memoization.

--p 0 = 1
--p 0 = 1
--p n = sum [f(k+1)*p(n-k*(3*k-1)`div`2)|k<-(takeWhile (\k -> n-k*(3*k-1)`div`2 >= 0) [1..])++(takeWhile (\k -> n-k*(3*k-1)`div`2 >= 0) [-1,-2..]),let f x = if even x then 1 else -1]
--should be better? time to test
ps :: Int -> Integer
ps = (map p' [0..] !!)
  where p' 0 = 1
        p' 1 = 1
        p' n = sum [f (k + 1) * ps (n - k * (3 * k - 1) `div` 2) |
                      let f x = if even x then 1 else - 1,
                      k <- takeWhile (\ k -> n - k * (3 * k - 1) `div` 2 >= 0) [1 .. ]
                             ++
                               takeWhile
                                  (\ k -> n - k * (3 * k - 1) `div` 2 >= 0) [- 1, - 2 .. ]]

qs :: Int -> Integer
qs = (map p' [0..] !!)
  where p' 0 = 1
        p' 1 = 1
        p'  n =(sum [sumDivisors (n-k)*qs (fromIntegral k)|k<-[0..n-1]]) `div` n

main::IO()
main = do
  putStrLn "Let p(n) = number of partitions overall. Here's p n for a few values to confirm.."
  mapM_ (\n -> do
    putStr ("p("++show n++ ") = ")
    print $ ps n) [0..20]
  putStr "p(100) = "
  print ( ps 100)

  putStrLn $ "p(100) - 1 = "++show (ps 100 -1)++" is the amount with at least two integers."