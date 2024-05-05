module PrjEuler60 (candidates, test, main) where

import Primes (primes, isPrime, undigits, digits)

candidates :: Int -> [[Int]]
candidates n = collect 5 [] initPrimes
  where
    initPrimes = takeWhile (<=n) primes

collect    :: Int -> [Int] -> [Int]  -> [[Int]]
collect 0 acc _ = [reverse acc]
collect k acc (p:ps)
    | all (test p) acc = collect (k-1) (p:acc) ps ++ collect k acc ps
    | otherwise = collect k acc ps
collect _ _ [] = []

test :: Int -> Int -> Bool
test x y =
  let (dx, dy) = (digits x, digits y)
      (xy, yx) = (undigits (dx++dy), undigits (dy++dx))
  in isPrime xy && isPrime yx

-- It can be shown that 'length (candidates 10000) == 1', 
-- so we can safely take the head of it.
-- Computing the full list once was a proof that the solution is unique.
-- Not computing the entire list on a run for the answer saves time. 
presolution :: [Int]
presolution = head . candidates $ 10000

solution :: Int
solution = sum presolution

main:: IO()
main = do
  print presolution
  print solution