solution = sum $ map (\x -> read[x]::Int) $ show (product [1..100])

main :: IO()
main = do
  print solution