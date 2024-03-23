import Primes (digitsBase)
{-- Find the sum of all numbers < 10^6 which are palindromic in base 10 and base 2.

Example: 585 = 1001001001_2
--}

{-- Analysis/explanation
Generate palindromes of a given length by taking numbers with half as many 
digits, and combining them with their reverse.

We must consider two cases: odd and even length palindromes.

-- Odd case
Call the number of digits n = 2k+1. We must create a generator for the
first half of the palindrome, and then combine it later.

Note n `div` 2 = k, which will not cover the middle digit.

Use k+1 digits. Call p = (n+1) `div` 2 = (2k + 2) `div` 2 = k+1

10^m has m+1 digits, so 10^(p-1) has p digits, and 10^p - 1 has p digits, and
that will be the range.

E.g., for 5 digits, we need (5+1) `div` 2 = 3 digits, so the range is [100..999], 
and our combining function will map xyz -> xyzyx

-- Even case
If the number of digits n = 2k, then n `div` 2 = k, k digits will suffice.

Let p = n `div` 2 = 2k `div` 2 = k.
Again, 10^m has m+1 digits, so 10^(p-1) has p digits, and 10^p - 1 has p digits.

E.g., for 4 digits, we need 4 `div` 2 = 2 digits, so the range is [10..99], and
our combining function will map xy -> xyyx.
-}
palindromesLength :: Int -> [Int]
palindromesLength n
  | odd n = 
      let p = (n+1) `div` 2 -- [1,3,5] -> [1,2,3]
      in oddCombine <$> [10^(p - 1)..10^p - 1] -- [10^(2-1)..10^2-1] -> [10..99] is for 3 digits
  | otherwise = 
      let p = n `div` 2 -- [2,4,6] -> [1,2,3]
      in evenCombine <$> [10^(p - 1)..10^p - 1] -- [10^(2-1)..10^2-1] -> [10..99] is for 4 digits
 where
   oddCombine x = 
     let (s, s') = (show x, drop 1 $ reverse s) -- 123 -> ("123", "21")
     in read $ s ++ s' -- "123" ++ "21" -> 12321
   evenCombine x = 
     let (s, s') = (show x, reverse s) -- 123 -> ("123", "321")
     in read $ s ++ s' -- "123" ++ "321" -> 123321
    

palindromes :: [Int]
palindromes = palindromesLength =<< [1..6]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ds = ds == reverse ds

solution :: Int
solution = sum . filter (isPalindrome . digitsBase 2) $ palindromes

main :: IO()
main = do
  print solution
