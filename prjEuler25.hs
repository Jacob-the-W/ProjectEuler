fibs = 0:1:zipWith (+) fibs (tail fibs)
indexedFibs = zip [0..] fibs
main::IO()
main = do
  print  $ fst $ head $ dropWhile (\(i,f) -> f < 10^999) indexedFibs -- 10^n has n+1 digits
