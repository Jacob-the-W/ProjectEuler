module PrjEuler65 where
import Data.Ratio
import Data.Char

-- only need the 100th convergent

e :: [Ratio Integer]
e = 2 : concatMap (\k -> [1,2*k,1] ) [1..33]

convergent :: [Ratio Integer] -> Ratio Integer
convergent = foldr1 (\x y -> x + 1/y)

solution :: Int
solution = sum digits where
  digits = map digitToInt . show . numerator . convergent $ e
  
main :: IO()
main = do
  print solution