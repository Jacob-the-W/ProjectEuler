import Primes (digits)

list = [a^b|a<-[2..100],b<-[2..100]]
solution = maximum $ sum . digits <$> list

main :: IO()
main = do
  print solution