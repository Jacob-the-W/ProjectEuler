module PrjEuler58 where
import Primes (isPrime)

numbersOnDiagonal :: [(Int, Int)]
numbersOnDiagonal = (1,1):[(n,n^2-k*(n-1))|n<-[3,5..],k<-[0..3]] 

proportionMaker :: [(Int, Int, Int)]
proportionMaker = drop 2 $ go 0 0 numbersOnDiagonal where -- drop 2 for trivial solutions
  go accL accR ((a,b):xs) =
    let (l, r) = if isPrime b then (accL + 1, accR + 1) else (accL + 1, accR)
    in (l, r, a):go l r xs

solution :: [(Int, Int, Int)]
solution = take 1 . dropWhile (\(l,r, n) -> 10*r >= l) $ proportionMaker -- r / l <= 1/10 is the same as 10*r <= l

main :: IO ()
main = do print solution