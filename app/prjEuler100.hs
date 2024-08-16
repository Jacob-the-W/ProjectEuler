module PrjEuler100 where

import Data.Ratio

{-
Playing loose with limits,

a/(a+b) * (a-1)/(a+b-1) -> 1/2 
  implies 
  a/(a+b) -> 1/sqrt 2 = 1/(1 + sqrt 2 - 1), 

so a/b -> 1/(sqrt 2 - 1) = sqrt 2 + 1 

Find the convergent of 1 + sqrt 2  and scale up til it works


solving ak/(ak + bk) * (ak - 1)/(ak + bk - 1) = 1/2 for k
 k = (a - b)/(a^2 - 2*a*b - b^2)
-}

convergent :: Fractional a => [a] -> Int -> a
convergent xs n = foldr1 (\x y -> x + 1/y) (take n xs)

solution :: (Integer, Integer)
solution =  head . filter ((>10^12) . uncurry (+)) . map scale $ 
  convergent (toRational <$> repeat 2) <$> [1..] 
    where
      scale r = 
        let (a, b) = (numerator r, denominator r)
            k = (a - b) `div` (a^2 - 2*a*b - b^2)
        in (a*k, b*k)

main :: IO ()
main = do 
    print solution
    putStrLn (show (fst solution) ++ " blue discs.")