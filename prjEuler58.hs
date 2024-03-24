module Main (main, proportionMaker) where
import Primes (primes')

isPrime :: Int -> Bool
isPrime n =
  let r = floor . sqrt $ fromIntegral n
  in (n>=2) && all (\p -> n `mod` p /= 0) (takeWhile (<=r) primes')

numbersOnDiagonal :: [(Int, Int)]
numbersOnDiagonal = (1,1):[(n,n^2-k*(n-1))|n<-[3,5..],k<-[0..3]] 

proportionMaker :: [(Int, Int, Int)]
proportionMaker = drop 2 $ go 0 0 numbersOnDiagonal where
  go accL accR ((a,b):xs)
    | isPrime b =
       let (l, r) = (accL+1, accR + 1)
       in (l, r, a):go l r xs
    | otherwise =
       let (l,r) = (accL+1, accR)
       in (l, r, a): go l r xs

solution :: [(Int, Int, Int)]
solution = take 1 . dropWhile (\(l,r, n) -> 10*r >= l) $ proportionMaker

main :: IO ()
main = do print solution