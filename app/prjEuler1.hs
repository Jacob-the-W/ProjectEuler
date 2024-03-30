sets a b c = [a,c `div` a,b,c `div` b, lcm a b, c `div` lcm a b]
solution = 
  let [a,a',b,b',c,c']=sets 3 5 999 
  in (a'*a*(1+a') + b'*b*(1+b') - c'*c*(1+c')) `div` 2
solution2 = sum [x|x<-[1..999999],x `mod` 3 == 0|| x `mod` 5 == 0]

main :: IO()
main = do
  print solution

