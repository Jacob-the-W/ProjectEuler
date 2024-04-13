
module PrjEuler60 (candidates, test, main)where
import Primes (primes, isPrime, undigits, digits)

candidates :: Int -> [[Int]]
candidates n=
  [ps|let primes1 = takeWhile (<=n) primes,
     p1 <- primes1,
     let primes2 = dropWhile (<= p1) primes1,
     p2 <- primes2, test p1 p2,
     let primes3 = dropWhile (<= p2) primes2,
     p3 <- primes3, all (test p3) [p1, p2],
     let primes4 = dropWhile (<= p3) primes3,
     p4 <- primes4, all (test p4) [p1, p2, p3],
     let primes5 = dropWhile (<= p4) primes4,
     p5 <- primes5, all (test p5) [p1, p2, p3, p4],
     let ps = [p1,p2,p3,p4,p5]]

test :: Int -> Int -> Bool
test x y =
  let (dx, dy) = (digits x, digits y)
      (xy, yx) = (undigits (dx++dy), undigits (dy++dx))
  in isPrime xy && isPrime yx

presolution :: [Int]
presolution = head $ candidates $ until (not . null . candidates) (10*) 1

solution :: Int
solution = sum presolution

main:: IO()
main = do
  print presolution
  print solution
-- print $ candidates (maximum presolution)