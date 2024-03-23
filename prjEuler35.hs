import Primes (primes')

potentials = [2,5] ++ filter (all (`elem` "1379") . show) (takeWhile (<=10^6) primes')
-- every prime past 10 must end in a 1, 3, 7, or 9. Cycling any other digit breaks it.

isPrime n =
    let r = floor . sqrt . fromIntegral $ n
    in all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes')

myCycle n = 
  let digits = floor $ logBase 10 (fromIntegral n) -- technically digits + 1
      (q, r) = n `quotRem` (10^digits)
  in 10*r + q

isPrimeCycle n = 
  let steps@(i:is) = take 6 (iterate myCycle n)
      cycles = i:takeWhile isPrime is
  in length cycles == 6


solution::Int
solution = length (filter isPrimeCycle potentials)

main::IO()
main = do
  print solution
