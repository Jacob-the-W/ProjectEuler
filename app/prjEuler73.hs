module PrjEuler73 where
  
mediant :: (Int, Int) -> (Int, Int) -> (Int, Int)
mediant (a,b) (c,d) = (a+c, b+d)

f :: [(Int,Int)] -> Int -> [(Int, Int)]
f [] _ = []
f [x] _ = [x]
f (x:y:xs) n
  | q > n = x : f (y:xs) n
  | otherwise = f (x:m:y:xs) n
  where m@(_,q) = mediant x y

solution :: Int
solution = subtract 2 . length $ f [(1,3),(1,2)] 12000

main :: IO ()
main = do print solution