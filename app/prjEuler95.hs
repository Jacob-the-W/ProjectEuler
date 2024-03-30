module PrjEuler95 where

import Primes (sumPropDivisors)

checkLoop :: [Int] -> [Int]
checkLoop = go [] where
  go acc [] = acc
  go [] (x:xs) = go [x] xs
  go acc (x:xs)
    | x>1000000 || x `elem` [0,1] = []
    | x `elem` acc = x:acc
    | otherwise = go (x:acc) xs

presolution :: [(Int, Int)]
presolution = map count . filter formsChain $ 
  checkLoop . scrubDeficient . iterate sumPropDivisors <$> [2..1000000] where
    count (x:xs) = (length xs, x)  -- [6,6] -> (1,6)
    count _ = error "Empty list" -- should never fire
    scrubDeficient (x:y:xs) = if x <= y then x:y:xs else []
    scrubDeficient _ = error "Iterating sumPropDivisors should never return an empty list"
    formsChain ys = case ys of
      [] -> False
      (x:_) -> last ys == x

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