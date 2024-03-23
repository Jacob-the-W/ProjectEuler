{--Problem 37
The number 3797 has an interesting property. 
Being prime itself, it is possible to continuously remove digits from left to 
right, and remain prime at each stage: 3797, 797, 97, and 7. 

Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
--}

import Primes (primes' ,isPrime)

-- cant start or end with 1,4,6,8,9. cant end with 2 or 5. 23 -> 2 or 3 works, smallest example.
--Can't use 0 in the middle 302 -> 30 on right truncation for example

filter1 :: [Int]
filter1 = filter (\x -> head (show x) `notElem` "14689") primes'
filter2 :: [Int]
filter2 = filter (\x -> last (show x) `notElem` "1245689") filter1
filter3 :: [Int]
filter3 = filter (\x -> '0' `notElem` take (length (show x) - 1) (drop 1 (show x))) filter2

leftTruncate :: Show a => a -> Int
leftTruncate n = read (tail (show n))::Int

rightTruncate :: Int -> Int
rightTruncate n = n `div` 10

isLeftTruncatable :: Int -> Bool
isLeftTruncatable n = all isPrime (take (length (show n)) (iterate leftTruncate n))

isRightTruncatable :: Int -> Bool
isRightTruncatable n = all isPrime (take (length (show n)) (iterate rightTruncate n))

solution :: [Int]
solution = take 11 $ filter (>=23) $ filter isLeftTruncatable $ filter isRightTruncatable filter3

main :: IO()
main = do
  putStrLn $ show solution++" has sum "++show (sum solution)