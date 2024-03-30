module Main (main, sumPropDivisors, checkLoop) where

import Data.List (group)

primes :: [Int]
primes = 2 : filter isPrime wheel where
  isPrime n =
    let r = floor . sqrt . fromIntegral $ n
    in all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes)
  wheel  = 3:5:7: drop 1 ( scanl (+) 1 (cycle wheel'))
  wheel' = [10,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,
           2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2]

primeFactorization :: Int -> [Int]
primeFactorization n = factors n primes
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

primeWithPowers :: Int -> [(Int, Int)]
primeWithPowers = map (\(p:ps) -> (p, length $ p:ps)) . group . primeFactorization

sumDivisors :: Int -> Int
sumDivisors n = case n of
  0 -> 0
  _ -> foldl (*) 1 . map (\(a,b)-> (a^(b+1)-1) `div` (a-1)) . primeWithPowers $ n

sumPropDivisors :: Int -> Int
sumPropDivisors n = sumDivisors n - n

checkLoop :: [Int] -> [Int]
checkLoop = go [] where
  go acc [] = acc
  go [] (x:xs) = go [x] xs
  go acc (x:xs)
    | x>10^6 || x `elem` [0,1] = []
    | x `elem` acc = x:acc
    | otherwise = go (x:acc) xs

presolution :: [(Int, Int)]
presolution = map count . filter formsChain $ 
  checkLoop . scrubDeficient . iterate sumPropDivisors <$> [2..10^6] where
    count (x:xs) = (length xs, x)  -- [6,6] -> (1,6)
    scrubDeficient (x:y:xs) = if x <= y then x:y:xs else []
    formsChain ys = case ys of
      [] -> False
      (x:xs) -> last ys == x

-- Outputs the high-records
solution :: [(Int, Int)]
solution = score presolution where
    score [] = []
    score (x@(a,_):xs) = x:score (dropWhile ((<=a) . fst) xs)

main :: IO()
main = do
  print (snd . last $ solution)
  putStrLn "Here's the list of [(chain length, number)]:"
  print presolution
  putStrLn "Rolling max:"
  print solution