solution :: Integer
solution = foldl (\x y -> (x+y^y) `mod` (10^10)) 0 [1..1000]

main :: IO ()
main = print solution