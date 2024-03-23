import Primes (primes')
import Data.List ( permutations )

isPrime :: Int -> Bool
isPrime n = 
    let r = floor . sqrt . fromIntegral $ n 
    in all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes')

pandigitalNumbers :: Int -> [Int]
pandigitalNumbers nDigits  = read <$> (permutations . take nDigits $ "123456789")

pandigitalPrimes :: Int -> [Int]
pandigitalPrimes = filter isPrime . pandigitalNumbers

solution :: Int
solution = maximum $ pandigitalPrimes =<< [1..9]

main :: IO()
main = do
  print solution
