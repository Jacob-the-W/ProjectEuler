module PrjEuler4 (main) where

products :: [Int]
products = [x*y|x<-[100..999], y<-[x..999]]

isPalindrome :: Int -> Bool
isPalindrome n = 
  let s = show n 
  in s == reverse s

solution :: Int
solution = maximum . filter isPalindrome $ products

main :: IO ()
main = print solution