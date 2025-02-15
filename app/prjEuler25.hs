module PrjEuler25 (main) where

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

solution :: Integer
solution = fst . head . dropWhile ((<10^999) . snd) . zip [0..] $ fibs -- 10^n has n+1 digits

main :: IO ()
main = print solution
