module PrjEuler59 (runTests, main) where

import Primes (digitsBase)
import Data.Char
import Data.List

xor' :: Int -> Int -> Int
xor' x1 x2 =
  case (x1, x2) of
    (1, 0) -> 1
    (0, 1) -> 1
    _ -> 0
-- if x1+x2==1 && x1*x2==0 then 1 else 0

xor :: Int -> Int -> Int
xor a1 a2 =
 let x1 = digitsBase 2 a1
     x2 = digitsBase 2 a2
     (n1, n2, n) = (length x1, length x2, max n1 n2)
     (x1', x2') = (replicate (n - n1) 0 ++ x1, replicate (n-n2) 0 ++ x2)
 in foldl1 (\x y -> 2*x + y) $ zipWith xor' x1' x2'

threeLetterStrings :: [String]
threeLetterStrings = [[a,b,c]|a<-['a'..'z'],b<-['a'..'z'],c<-['a'..'z']]

test :: [Int] -> [Int] -> [Int]
test c letters = zipWith xor c (cycle letters) -- c is the cipher

runTests :: [Int] -> [([Char], [Char])]
runTests c = dropWhile (\(y, _)->not (" the " `isInfixOf` y)) $ (\x -> let y = test c x in y `seq` (map chr y,map chr x)) . map ord <$> threeLetterStrings

main :: IO()
main = do
  input <- readFile "data\\p059_cipher.txt"
  let cipher = read ("[" ++ input ++ "]")::[Int]
  let tests = runTests cipher
      (decoded, key) = case tests of
        [] -> error "No solution found"
        (d, k):_ -> (d, k)
  print decoded;
  putStrLn $ "\nKey: " ++ key ++ "\n";
  let solution = sum . map ord $ decoded
  print solution
  --brute force found "exp" as key.
  --print solution