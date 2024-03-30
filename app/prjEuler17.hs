module PrjEuler17 where

lengthInEnglish :: Int -> Int
lengthInEnglish x
    | x == 0  = 4 -- zero has 4 letters
    | x == 1  = 3 -- one has 3
    | x == 2  = 3 -- two has 3
    | x == 3  = 5 -- ..
    | x == 4  = 4
    | x == 5  = 4
    | x == 6  = 3
    | x == 7  = 5
    | x == 8  = 5
    | x == 9  = 4
    | x == 10 = 3
    | x == 11 = 6
    | x == 12 = 6
    | x == 13 = 8
    | x == 14 = 8
    | x == 15 = 7
    | x == 16 = 7
    | x == 17 = 9
    | x == 18 = 8
    | x == 19 = 8
    | x == 20 = 6
    | x == 30 = 6
    | x == 40 = 5
    | x == 50 = 5
    | x == 60 = 5
    | x == 70 = 7
    | x == 80 = 6
    | x == 90 = 6
    --between 21-99, basically doing length "twenty" plus length "one", but catch that we don't say "twentyzero"
    | x > 20 && x < 100  && mod x 10 /= 0 = lengthInEnglish (10*div x 10) + lengthInEnglish (mod x 10)
    | x > 20 && x < 100  && mod x 10 == 0 = lengthInEnglish (div x 10)
    --problem uses British "onehundredandtwentyone", reduce it to "one" "hundred" "and" "twentyone" 
    --where we recursively get the twentyone from previous lines
    | x >= 100 && x < 1000 = if mod x 100 /= 0
                             then lengthInEnglish (div x 100) + 7 + lengthInEnglish (mod x 100)+3
                             -- +3 because of the "and" between hundred
                             else lengthInEnglish (div x 100) + 7
    --length "onethousand" == 11.
    --Could keep defining recursively, but lazy and problem only needs up to 1000.
    | x == 1000 = 11
    | otherwise = 0

solution :: Int
solution = sum [lengthInEnglish x|x<-[1..1000]]

main :: IO()
main = do
  print solution