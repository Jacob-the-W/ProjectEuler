import Primes (primes)

main :: IO ()
main = do
  print (primes !! 10000)