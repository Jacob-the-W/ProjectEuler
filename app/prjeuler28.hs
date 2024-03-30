module PrjEuler28 where

-- All numbers except 1 are of the form n^2-k(n-1) for k=0..3, where n is odd. 
-- Examine the corners of each square to convince yourself.

solution :: Int
solution = sum $ 1:[n^2-k*(n-1)|n<-[3,5..1001],k<-[0..3]]

main::IO()
main = do
  print solution