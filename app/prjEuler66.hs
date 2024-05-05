module PrjEuler66 where

import Data.Ratio ( denominator, numerator )
import Data.List ( (\\) )

continuedFrac :: Integral t => t -> t -> [t]
continuedFrac a b =
  let (p,q) = a `divMod` b
  in if q==0 then [p] else p:continuedFrac b q

convergent :: Real a => [a] -> Int -> Rational
convergent xs n = foldr1 (\x y -> x + 1/y) (map toRational $ take n xs)

newton :: Fractional a => a -> a -> a
newton a x = 1/2*(x+a/x)

f :: Integer -> (Integer, Integer, Integer)
f d = findSolution . generateConvergents . continuedFraction $ initialGuess
  where
    initialGuess = (!!6) $ iterate (newton target) start
      where
        target = toRational d
        start = toRational $ floor $ sqrt (fromIntegral d)
    continuedFraction r = continuedFrac (numerator r) (denominator r)
    generateConvergents xs = map (convergent xs) (fst <$> zip [1..] xs)
    findSolution = (\(x, y)->(x, d, y)) . head . filter (\(x,y)->x^2-d*y^2==1) . map (\r->(numerator r, denominator r))

solution :: Integer
solution = (\(_ ,d, _)->d) . maximum $ map f nonSquares
  where
    nonSquares = [1..1000] \\ map (^2) [1..floor $ sqrt 1000]

main :: IO()
main = do
    print solution