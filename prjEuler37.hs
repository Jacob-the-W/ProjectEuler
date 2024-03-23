{--Problem 37
The number 3797 has an interesting property. 
Being prime itself, it is possible to continuously remove digits from left to 
right, and remain prime at each stage: 3797, 797, 97, and 7. 

Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
--}

import Primes (primes')

isPrime :: Int -> Bool
isPrime n =
  let r = floor . sqrt . fromIntegral $ n
  in all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes')

-- cant start or end with 1,4,6,8,9. cant end with 2 or 5. 
-- 23 -> 2 or 3 works, smallest example.
-- Can't use 0 in the middle 302 -> 30 on right truncation for example

test1 :: String -> Bool
test1 x = head x `notElem` "14689"

test2 :: String -> Bool
test2 x = last x `notElem` "1245689"

test3 :: String -> Bool
test3 x = '0' `notElem` take (length x - 1) (drop 1 x)

tests :: Int -> Bool
tests x =
  and [test1 s, test2 s, test3 s,
    isLeftTruncatable x, isRightTruncatable x]
  where
  (s, l) = (show x, length s)
  isLeftTruncatable = all isPrime . take l . iterate leftTruncate
  isRightTruncatable = all isPrime . take l . iterate rightTruncate

leftTruncate :: Int-> Int
leftTruncate = read . tail . show

rightTruncate :: Int -> Int
rightTruncate = (`div` 10)

presolution :: [Int]
presolution = take 11 $ dropWhile (<23) $ filter tests primes'

solution :: Int
solution = sum presolution
main :: IO()
main = do
  putStrLn $ show presolution++" has sum "++show solution
