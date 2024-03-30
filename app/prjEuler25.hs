module PrjEuler25 where

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

indexedFibs :: [(Integer, Integer)]
indexedFibs = zip [0..] fibs

solution :: Integer
solution = fst $ head $ dropWhile (\(_,f) -> f < 10^999) indexedFibs -- 10^n has n+1 digits

main :: IO ()
main = do print solution
