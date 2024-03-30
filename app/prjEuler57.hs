module PrjEuler57 where
{--
3/2, 7/5, 17/12, 41/29, ..
 
a 1 = 3
a n = 2*b (n-1)+a (n-1)
b 1 = 2  
b n = a (n-1) + b (n-1)
--}
fractionStepper :: Num b => (b, b) -> (b, b)
fractionStepper (a,b) = (2*b+a,a+b)

solution :: [(Integer, Integer)]
solution = filter (\(a,b)->length (show a) > length (show b)) $ take 1000 $ iterate fractionStepper (3,2)

main :: IO()
main = do
  putStrLn "First few examples.."
  print $ take 10 solution
  putStrLn "Length of list:"
  print $ length solution