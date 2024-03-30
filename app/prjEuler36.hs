
import Primes (digitsBase)
{-- 
Find the sum of all numbers < 10^6 which are palindromic in base 10 and base 2.
Example: 585 = 1001001001_2
--}

-- For each number 1..999, generate an odd and even length palindrome by 
-- concatenating the number with its reverse, and the number with its reverse
-- minus the first digit.  999 -> 999999, 99999
palindromes :: [Int]
palindromes = flipper =<< [1..999] :: [Int] where
  flipper x =
    let (s, r, c) = (show x, reverse s, drop 1 r)
    in read . (s ++) <$> [r,c]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ds = ds == reverse ds

solution :: Int
solution = sum . filter (isPalindrome . digitsBase 2) $ palindromes

main :: IO()
main = do
  print solution
