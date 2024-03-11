main :: IO()
main = do
  print $ foldl1 lcm [1..20]