module PrjEuler63 where
{- | nDigits (x^k) = floor (logBase 10 (x^k)) + 1 == k
floor (logBase 10 (x^k))  == k - 1
k - 1 <= logBase 10 (fromIntegral x) < k
10^(k-1) <= x^k < 10^k
10^((k-1)/k) <= x < 10

If the left is integer, floor left == ceiling left, and if not,
  minimum possible x = ceiling left, so use ceiling.
-}

kDigitPower :: Int -> [Integer]
kDigitPower k = map (^k) [ceiling $ 10**(fromIntegral (k-1)/fromIntegral k)..9]

{- | How many powers make sense?

10^n has n+1 digits (easy).

9^n = 10^(n*logBase 10 9) has 
  n*logBase 10 9 + 1 
    digits

For 9^n to have n digits, floor (n*logBase 10 9) + 1 == n

n - 1 <= n*logBase 10 9 < n
1 - 1/n <= logBase 10 9 < 1
The right is equivalent to 9 < 10
Right side is always true, what about left?
1 - logBase 10 9 <= 1/n
n <= 1/(1 - logBase 10 9)
n <= log 10/(log $ 10/9)
n <= 21.85 
n <= 21, n is an integer
-}
list :: [Integer]
list =  kDigitPower =<< [1..21]

solution :: Int
solution = length list

main :: IO ()
main = do
  print list
  putStrLn "Length of the above list:"
  print solution