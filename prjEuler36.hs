-- 585 = 1001001001_2
--Find the sum of all numbers < 10^6 which are palindromic in base 10 and base 2.
{--
--x
palindromes 1 = [1..9]
--xx
palindromes 2 = [11,22,33,44,55,66,77,88,99]
--xyx
palindromes 3 = map (\x -> read(x)::Integer) $ map (\x -> x ++ (drop 1 $ reverse x)) $ map show [x|x<-[10..99]]
--xyyx
palindromes 4 = map (\x -> read(x)::Integer) $ map (\x -> x ++ (reverse x)) $ map show [x|x<-[10..99]]
--xyzyx
palindromes 5 = map (\x -> read(x)::Integer) $ map (\x -> x ++ (drop 1 $ reverse x)) $ map show [x|x<-[100..999]]
--xyzzyx
palindromes 6 = map (\x -> read(x)::Integer) $ map (\x -> x ++ (reverse x)) $ map show [x|x<-[100..999]]
--}

--generalized based on above pattern
palindromes n =
  if n `mod` 2 == 1
  then  map (((\x -> read x::Integer) . (\x -> x ++ drop 1 (reverse x))) . show) [10 ^ ((n + 1) `div` 2 - 1) .. 10 ^ ((n + 1) `div` 2) - 1]
  else  map (((\x -> read x::Integer) . (\x -> x ++ reverse x)) . show) [10 ^ (n `div` 2 - 1) .. 10 ^ (n `div` 2) - 1]

palindromesUnderBound = concat [palindromes n|n<-[1..6]]

isPalindrome x = show x == reverse (show x)

intToBase k n = read $ (concatMap show . reverse) $ map (`mod` k) $
    takeWhile (>0) [n `div` k^i|i<-[0..]]::Integer


digits n = map (\x->read [x]::Integer) $ show n
baseKtoInt k knary = sum $ zipWith (*) (map (k ^) [0..]) (reverse $ digits knary)

solution = sum $ map (baseKtoInt 2) $ filter isPalindrome $ map (intToBase 2) palindromesUnderBound


main :: IO()
main = do
  print solution