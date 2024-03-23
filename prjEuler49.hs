import Primes (primes')
import Data.List (group,sort)

isPrime n = 
  let r = floor . sqrt $ fromIntegral n
  in all (\p -> n `rem` p /=0) . takeWhile (<=r) $ primes'

candidates = takeWhile (<10000) $ dropWhile (<1000) primes'

-- if pi > pj let n = pi -pj, need to check pj - n = pj - (pi - pj) = 2*pj - pi is prime
isPermutations = null . drop 1 . group . map (sort . show)

solution :: Int
solution =  read $ concatMap show (reverse . last $ [[a,b,c]| i<-[0..length candidates-1],j<-[0..(i-1)],
        let a = candidates !! i; b = candidates !! j; c = 2*b-a;,
        isPrime c, isPermutations [a,b,c]])

main :: IO()
main = do
  print solution
