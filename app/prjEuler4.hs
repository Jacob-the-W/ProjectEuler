threeDigitNums = [100..999]
products = [x*y|x<-threeDigitNums,y<-threeDigitNums]
solution = maximum $ filter (\x -> show x == reverse (show x)) products

main :: IO()
main = do
  print solution