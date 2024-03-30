{-# LANGUAGE TupleSections #-}
module PrjEuler29 where
import qualified Data.Map as Map

solution :: Int
solution = length . Map.keys . Map.fromList . map (,()) $ [a^b|a<-[2..100],b<-[2..100]]

main::IO()
main = do
  print solution 