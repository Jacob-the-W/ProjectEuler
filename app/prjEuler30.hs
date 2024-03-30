import Data.List

sumOfPowers xs = sum $ map (^5) xs

-- 999999  -> 6*9^5 = 354294, the biggest 6 digit number can only get mapped up to a 6 digit number
-- 9999999 -> 7*9^5 = 413343, the biggest 7 digit number can only get mapped up to a 6 digit number 
numbers5digits = [11..354294]
fixedList = map ((sumOfPowers . map (\x -> read [x]::Integer)) . show) numbers5digits
filteredList = map fst . filter (uncurry (==)) $ zip numbers5digits fixedList
solution = sum filteredList
--zip numbers5digits fixedList -- the list of numbers 




main::IO()
main = do
  print filteredList
  print solution