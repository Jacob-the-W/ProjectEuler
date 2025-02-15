module PrjEuler17 (main) where

lengthInEnglish :: Int -> Int
lengthInEnglish x
    | x `elem` [0, 4, 5, 9]  = 4 -- zero has 4 letters
    | x `elem` [1, 2, 6, 10]  = 3 -- one has 3
    | x `elem` [3, 7, 8, 40, 50, 60]  = 5 -- ..
    | x `elem` [11, 12, 20, 30, 80, 90] = 6
    | x `elem` [13, 14, 18, 19] = 8
    | x `elem` [15, 16, 70] = 7
    | x == 17 = 9
    --between 21-99, basically doing length "twenty" plus length "one", but catch that we don't say "twentyzero"
    | x > 20 && x < 100 =
      let (q, r) = x `divMod` 10
      in lengthInEnglish (10*q) + if r == 0 then 0 else lengthInEnglish r
    --problem uses British "onehundredandtwentyone", reduce it to "one" "hundred" "and" "twentyone" 
    --where we recursively get the twentyone from previous lines
    | x >= 100 && x < 1000 = 
      let (q, r) = x `divMod` 100
      in if r == 0 then lengthInEnglish q + 7 else sum $ 10:(lengthInEnglish <$> [q, r])
    --length "onethousand" == 11.
    --Could keep defining recursively, but lazy and problem only needs up to 1000.
    | x == 1000 = 11
    | otherwise = 0

solution :: Int
solution = sum $ lengthInEnglish <$> [1..1000]

main :: IO()
main = print solution