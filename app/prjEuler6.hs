module PrjEuler6 (main) where

-- These can be proven by induction or found with generating functions.
-- sumSquares :: Integral a => a -> a
-- sumSquares n = n*(1+2*n)*(1+n) `div` 6

-- squareSum :: Integral a => a -> a
-- squareSum n = (n*(n+1)`div`2)^2

solution1 :: (Num t, Enum t) => t -> t
solution1 n = sum [1..n]^2 - sum [x^2|x<-[1..n]]

solution2 :: Integral a => a -> a
-- solution n =  squareSum n - sumSquares n
solution2 n = (n-1)*n*(n+1)*(3*n+2) `div` 12

main :: IO ()
main = do
    mapM_ print ((\n -> (n, solution1 n)). (10^) <$> [0..6])
    mapM_ print ((\n -> (n, solution2 (10^n))) <$> [0..6])