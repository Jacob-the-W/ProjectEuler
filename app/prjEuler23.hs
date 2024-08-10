module PrjEuler23 where

import Primes (abundantsTo)
import Data.Array.Unboxed

solution :: Int
solution = 
  let empties = listArray (1,28123) (repeat 0) :: UArray Int Int
      as = abundantsTo 28123
      updated = empties // [(i+j,1)|i<-as, j<-takeWhile (<=28123-i) as]
  in sum [i|(i, check)<-assocs updated, check == 0]

main :: IO ()
main = print solution