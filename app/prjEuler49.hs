module PrjEuler49 where

import Primes (primes, isPrime)
import Data.List (group,sort)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)

candidates :: IntMap Int
candidates = M.fromDistinctAscList . zip [0..] . takeWhile (<10000) $ dropWhile (<1000) primes

-- if pi > pj let n = pi -pj, need to check pj - n = pj - (pi - pj) = 2*pj - pi is prime
isPermutations :: [Int] -> Bool
isPermutations = null . tail . group . map (sort . show)

solution :: Int
solution =  read $ show =<< last xs where
  xs = [[c,b,a]| i<-[0..length candidates-1],j<-[0..(i-1)],
        let a = candidates M.! i; b = candidates M.! j; c = 2*b-a;,
        isPrime c, isPermutations [a,b,c]]

main :: IO()
main = do
  print solution
