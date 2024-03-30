import Data.List

main::IO()
main = do
  print $ length $ nub [a^b|a<-[2..100],b<-[2..100]] -- not optimized, but [2..100]x[2..100] has 99*99*9801 elements, so not too horrible to nub.