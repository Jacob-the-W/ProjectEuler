module PrjEuler62 where
--Find the smallest cube for which exactly five permutations of its digits are cube.
--need at least five permutations allowable
--1!=1, 2! = 2, 3! = 6, so we must start at 3 digit cubes.
--smallest cube with 3 digits, 5^3=125
import Data.List

cubes :: [Int]
cubes = map (^3) [5..]

-- if n^3 is k digits, 10^k has k + 1 digits, so, 10^((k-1)/3) < n < 10^(k/3)
kdigitCubes::Int->[Int]
kdigitCubes k = map (^3) [floor (10**(fromIntegral (k-1)/3))..floor (10**(fromIntegral k/3))]

isPermutation :: Int -> Int -> Bool
isPermutation x x' =
  let (s, s') = (show x, show x')
  in null (s \\ s')

numberOfPermutationsInCubes :: Int -> Int
numberOfPermutationsInCubes x =
  let k = floor (logBase 10 (fromIntegral x)) + 1
  in length $ filter (`isPermutation` x) (kdigitCubes k)

solution :: Int
solution = head . filter ((==5) . numberOfPermutationsInCubes) $ cubes

main ::IO()
main = do
  print solution