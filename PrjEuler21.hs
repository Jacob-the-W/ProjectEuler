import Primes (amicablePairs)

solution = sum . takeWhile (not . null) . map (\(a,b)->filter (<=10000) [a,b]) $ amicablePairs

main = print solution
