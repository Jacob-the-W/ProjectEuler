import Data.List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

lychrel :: String -> String
lychrel x =  show (read x + read (reverse x))

setup :: [(String, Integer)]
setup = filter (not . isPalindrome . fst) $ zip (map (lychrel . show) [1..9999]) [1..9999]

f :: (String, b) -> (String, b)
f (a,b) = (lychrel a, b)

solution :: [(String, Integer)]
solution = foldr (\x acc -> filter (not . isPalindrome . fst) $ map f acc)
  setup (replicate 23 ()) -- (replicate 50 ()) but 23 is enough for this specific case

main::IO()
main = do
  print . head $ solution
  print . length $ solution