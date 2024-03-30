pent :: [Int]
pent = map (\n->n*(3*n-1) `div` 2) [1..]
hex :: [Int]
hex = map (\n->n*(2*n-1)) [1..]
isHex :: Int -> Bool
isHex t = t `elem` takeWhile (<= t) (drop 142 hex)
result :: Int
result =last $ take 2 $
         filter isHex pent

main::IO()
main = do
  print result

--not great 
--filter (\t -> is `elem` (takeWhile (<= t) pent) [t|t<-(drop 284 tri)
-- filter (\t -> t `elem` (takeWhile (<= t) tri)) [t|t<-hex]