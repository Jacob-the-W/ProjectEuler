module PrjEuler45 where

pent :: [Int]
pent = map (\n->n*(3*n-1) `div` 2) [1..]
hex :: [Int]
hex = map (\n->n*(2*n-1)) [143..]
isHex :: Int -> Bool
isHex t = t `elem` takeWhile (<= t) hex
result :: Int
result = head. drop 1 $
         filter isHex pent

main::IO()
main = do
  print result

--not great 
--filter (\t -> is `elem` (takeWhile (<= t) pent) [t|t<-(drop 284 tri)
-- filter (\t -> t `elem` (takeWhile (<= t) tri)) [t|t<-hex]