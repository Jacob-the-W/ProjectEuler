-- 9! = 362880, we have to check at least up to 362880, 6*9! = 2177280, a 6 digit number can map to 7 digits, 7*9! = 2540160, so we'll consider that our upper bound.

import Data.Char

readDigits = map (map digitToInt . show)

nums = [3..2450160]
solutions = [n|n<-nums,[n]==sumOfFactorialOfDigits n]

sumOfFactorialOfDigits n = map (sum . map factorial) $ readDigits [n]

factorial 0 = 1
factorial 1 = 1
factorial n = product [1..n]

main::IO()
main = do
  print $ sum solutions
