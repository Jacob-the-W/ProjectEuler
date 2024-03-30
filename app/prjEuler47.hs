module PrjEuler47 where
  
import Primes (primePowers)

setup :: [[Int]]
setup = [[p,p+1,p+2,p+3] | p <- [1..]]

checking :: [Int]
checking = take 1 =<< filter (all ((4==) . length . primePowers)) setup

solution :: Int
solution = head checking

main::IO()
main = do
  print solution