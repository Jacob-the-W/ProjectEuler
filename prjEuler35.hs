primes = 2 : filter (\n -> all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..999999]
primes' = [2,5] ++ filter (all (`elem` "1379") . show) primes
-- every prime past 10 must end in a 1, 3, 7, or 9. Cycling any other digit breaks it.


myCycle::String->String
myCycle string = drop 1 string ++ take 1 string

isPrime n = let divisors =  filter (\p -> (n `mod` p) == 0) $ takeWhile (\p -> p*p <=n) primes
      in null divisors

isPrimeCycle n = let cycles = filter isPrime . map (\x -> read x::Int) $ take 6 (iterate myCycle (show n))
                 in length cycles == 6


solution::Int
solution = length (filter isPrimeCycle primes')

main::IO()
main = do
  print solution