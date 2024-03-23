list = map (\x -> read [x]::Int) $ concatMap show [1..]
d n = list !! max 0 (n-1)

solution = product [d (10^n)|n<-[0..6]]

main :: IO()
main = do
  print solution