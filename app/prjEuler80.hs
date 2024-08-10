module PrjEuler80 (main) where

import Data.Ratio
import Primes (primePowers)

newton :: (Fractional a) => (a -> a) -> (a -> a) -> a -> Int -> a
newton _ _ x 0 = x
newton f f' x n = newton f f' x' (n-1)
  where x' = x - (f x / f' x)

test :: Rational -> Rational
test n = newton (\x->x^2-n) (2 *) (fromIntegral . round . sqrt $ fromRational n) 8
--8 was arbitrarily picked after noticing,
--test 2 == 48926646634423881954586808839856694558492182258668537145547700898547222910968507268117381704646657 % 
--          34596363615919099765318545389014861517389860071988342648187104766246565694525469768325292176831232
-- hoped a length of 98 digits in the denominator guaranteed a difference of <10^-100 since 98 was so close to 100
--8 steps was actually the lowest that works, 7 steps returns 40879 and 8 steps returns the correct 40886.

fractionDigits :: Integral a => a -> a -> [a]
fractionDigits a b =
  let (p,q) = a `quotRem` b
  in if q == 0 then [p] else p:fractionDigits (10*q) b

sumDigits :: Rational -> Integer
sumDigits n = sum (take 100 (fractionDigits a b)) where
     a = numerator (test n)
     b = denominator (test n)

irrationals :: [Integer]
irrationals = filter (any (odd . snd) . primePowers) [1..100]

solution :: Integer
solution = sum $ map (sumDigits . fromIntegral) irrationals

main::IO()
main = do
  print solution