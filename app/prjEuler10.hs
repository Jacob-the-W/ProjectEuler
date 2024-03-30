import Primes (primes)

solution = sum $ takeWhile (<2*10^6) primes

main :: IO ()
main = do print solution