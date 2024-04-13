module PrjEuler59 (runTests, main, bestLettersFor) where


import Data.Char
import Data.List
import Data.Ord
import Data.Bits

-- "Letters" isn't quite right, 
-- as we're looking at the orders of the letters instead.
test :: [Int] -> [Int] -> [Int]
test cipher letters = zipWith xor cipher (cycle letters)

runTests :: [Int] -> [(String, String)]
runTests cipher = take 1 . dropWhile (noThe . snd) $ attempt <$> threeLetters where 
  threeLetters = permutations . bestLettersFor $ cipher
  attempt x = let y = test cipher (map ord x) in (x,map chr y)
  noThe = not . (" the " `isInfixOf`)

bestLettersFor :: [Int] -> [Char]
bestLettersFor cipher =
  take 3 . map fst . sortBy (comparing (Down . snd)) $
    (countSpace <$>) . pair <$> ['a'..'z'] where
  pair c = (c, c)
  countSpace = length . filter (== ord ' ') . test cipher . (:[]) . ord

main :: IO()
main = do
  input <- readFile "data\\p059_cipher.txt"
  let cipher = read ("[" ++ input ++ "]")::[Int]
  let tests = runTests cipher
      (key, decoded, solution) = case tests of
        [] -> ("", "", 0)
        (k, d):_ -> (k, d, sum . map ord $ d)
  print decoded;
  putStrLn $ "\nKey: " ++ key ++ "\n";
  print solution