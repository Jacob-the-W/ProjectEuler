import Data.List
{--
Since we must have more than one multiple, we can't take the biggest pandigital
 1-9 number and cheat.
So check up to 4 digits since 5000*2 = 10000, i.e., for some 4 digit numbers it
 is possible to concatenate up to a length of 9
--}
multiples n =  (n*) <$> [1..9]

isPanDigital n = sort (show n) == "123456789"

multsConcatToLength k n = elem k . scanl1 (+) $ length . show <$> multiples n

generator :: Int -> Int
generator n =
  if multsConcatToLength 9 n
  then read . take 9 $ show =<< multiples n
  else 0

solution = maximum . filter isPanDigital $ generator <$> [1..9999]

main :: IO()
main = do
  print solution
