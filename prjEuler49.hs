import Primes (primes')
import Data.List (group,sort)
import Data.IntMap qualified as M

isPrime n = 
  let r = floor . sqrt $ fromIntegral n
  in all (\p -> n `rem` p /=0) . takeWhile (<=r) $ primes'

candidates = M.fromDistinctAscList . zip [0..] . takeWhile (<10000) $ dropWhile (<1000) primes'

-- if pi > pj let n = pi -pj, need to check pj - n = pj - (pi - pj) = 2*pj - pi is prime
isPermutations = null . tail . group . map (sort . show)

solution :: Int
solution =  read $ show =<< last xs where
  xs = [[c,b,a]| i<-[0..length candidates-1],j<-[0..(i-1)],
        let a = candidates M.! i; b = candidates M.! j; c = 2*b-a;,
        isPrime c, isPermutations [a,b,c]]

main :: IO()
main = do
  print solution
