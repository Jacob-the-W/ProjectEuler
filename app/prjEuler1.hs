module PrjEuler1 (main) where

sets :: Integral a => a -> a -> a -> (a, a, a, a, a, a)
sets a b c = (a,c `div` a,b,c `div` b, lcm a b, c `div` lcm a b)

solution :: Int
solution = 
  let (a,a',b,b',c,c') = sets 3 5 999 
  in (a'*a*(1+a') + b'*b*(1+b') - c'*c*(1+c')) `div` 2

main :: IO ()
main = print solution

