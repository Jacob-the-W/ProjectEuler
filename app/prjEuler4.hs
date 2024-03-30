module PrjEuler4 where

products :: [Int]
products = let xs = [100..999] in [x*y|x<-xs,y<-xs]

isPalindrome :: Int -> Bool
isPalindrome n = 
  let s = show n 
  in s == reverse s

solution :: Int
solution = maximum . filter isPalindrome $ products

main :: IO()
main = do
  print solution