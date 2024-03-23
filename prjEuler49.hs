import Primes (primes,isPrime)
import Data.List
import qualified Data.IntMap as M

candidates = M.fromAscList . zip [0..] . takeWhile (<10000) $ dropWhile (<1000) primes

-- if pi > pj let n = pi -pj, need to check pj - n = is prime
isPermutations xs = 1 == length (nub $ map (sort . show) xs)

solution :: Int
solution =  read $ concatMap show (reverse . last $
  filter isPermutations $
    map (\(a,b,c) -> [a,b,b-c]) $ filter (\(a,b,c) -> isPrime $ b-c)
      [(candidates M.! i,candidates M.! j,candidates M.! i - candidates M.! j)|
                            i<-[0..length candidates-1],j<-[0..(i-1)]])

main :: IO()
main = do
  print solution
