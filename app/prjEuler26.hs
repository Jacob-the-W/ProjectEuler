module PrjEuler26 where

import Primes (primes, order)

import Data.List ( (\\) )

s :: Int
s = let start = takeWhile (<1000) (primes\\[2,5])
    in snd . maximum $ [(order 10 a, a)|a<-start]

main::IO()
main = do print s