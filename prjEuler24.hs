import Data.List

main::IO()
main = do
  print (sort (permutations "0123456789") !! 999999)