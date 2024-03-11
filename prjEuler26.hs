import Data.List
primes = 2 : filter (\n -> all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..]
s = map (\n -> (n,head [x | x <- [ceiling(logBase 10 (fromIntegral n))..], 10^x `mod` n == 1])) (takeWhile (<1000) (primes\\[2,5]))

main::IO()
main = do
  print s
  let maxB = maximum [b|(a,b)<-s]
  let result = head [(a,b)|(a,b)<-s, b == maxB]
  putStrLn(show (fst result) ++ " has a reciprocal with a repetition of length " ++ show (snd result))