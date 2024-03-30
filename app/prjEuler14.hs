module PrjEuler14 where
  
collatz :: Int -> [Int]
collatz n
  | n==1     = [1]
{-  
  | n==(-1)  = [-1,-2]
  | n==(-5)  = [-5,-14,-7,-20,-10]
  | n==(-17) = [-17,-50,-25,-74,-37,-110,-55,-164,-82,-41,-122,-61,-182,-91,-272,-136,-68,-34]
-}
  | even n =  n:collatz (n `quot` 2)
  | otherwise = n:collatz (3*n + 1)

solution :: Int
solution = snd . maximum $ (\n -> (length . collatz $ n, n)) <$> [1,3..10^6]

main :: IO()
main = do
  print solution