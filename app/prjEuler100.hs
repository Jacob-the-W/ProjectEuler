module PrjEuler100 where

-- Bad brute force solution, takes 26 hours to compute.
-- Look into Diophantine Equations
f :: [(Int, Int)]
f = [(a,b)|a<-[1..]
    , let test = 8*a^2-8*a+1
    , let r = floor . sqrt . fromIntegral $ test, r^2==test
    , let b = (r - 2*a+1) `div` 2]

solution = span (\(a,b)->a+b<=10^12) f

main :: IO()
main = do
    print . fst $ solution
    print . head . snd $ solution
