module PrjEuler56 where
import Primes (digits)

list :: [Integer]
list = [a^b|a<-[2..100],b<-[2..100]]
solution :: Integer
solution = maximum $ sum . digits <$> list

main :: IO()
main = do
  print solution