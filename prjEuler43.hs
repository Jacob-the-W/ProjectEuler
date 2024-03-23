import Data.List

primes :: [Int]
primes = [2,3,5,7,11,13,17]

panDigitals :: [Int]
panDigitals = map read $ filter (\x -> head x /= '0') (permutations "0123456789")

digitCheck :: Int -> Int -> Bool
digitCheck n k = check n k `mod` (primes!!(k-1)) == 0
  where check n k = read . take 3 . drop k $ show n :: Int

solution :: Int
solution = sum $ filter (\n-> all (digitCheck n) [1,2,3,4,5,6,7]) panDigitals

main :: IO ()
main = do print solution