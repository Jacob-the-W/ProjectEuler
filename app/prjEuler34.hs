module PrjEuler34 where
-- 9! = 362880, we have to check at least up to 362880, 6*9! = 2177280, a 6 digit number can map to 7 digits, 7*9! = 2540160, so we'll consider that our upper bound.

import Data.Char

readDigits :: Show a => a -> [Int]
readDigits n = digitToInt <$> show n

nums :: [Int]
nums = [3..2450160]

solutions :: [Int]
solutions = [n|n<-nums,n==sumOfFactorialOfDigits n]

sumOfFactorialOfDigits :: Show a => a -> Int
sumOfFactorialOfDigits n = sum . map factorial $ readDigits n

factorial :: Integral a => a -> a
factorial n = product [1..n]

main :: IO()
main = do
  print $ sum solutions
