import Primes (abundantsTo)
import Data.IntMap qualified as Map

solution = sum . Map.keys . Map.difference emptyMap $ sumMap
  where
    as = abundantsTo 28123
    emptyMap = Map.fromDistinctAscList (map (,()) [1..28128])
    sumMap = Map.fromList [(a+b,())|a <- as,  b <- takeWhile(<=28128-a) as]

main = print solution
