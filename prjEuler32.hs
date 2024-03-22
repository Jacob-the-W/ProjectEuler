
import Data.List

panDigitals::[Int]
panDigitals = map read (permutations "123456789")
--2,3,4 digits makes sense
--xx*yyy==zzzz case
first x = read (take 2 (show x))::Int
second x= read (take 3 (drop 2 (show x)))::Int
rest x  = read (drop 5 (show x))::Int
--x*yyyy=zzzz case?
first' x = read (take 1 (show x))::Int
second' x = read (take 4 (drop 1 (show x)))::Int
rest' x = read (take 4 (drop 5 (show x)))::Int
-- xxyy=zzzzz? 99 *99 = 9801, so impossible. 

case1 = [x |x<-panDigitals, first x *second x==rest x]
case2 = [x |x<-panDigitals, first' x *second' x==rest' x]
products = map (read . (drop 5 . show)) (case1 ++ case2)
uniqueProducts = map head . group $ products
solution = sum uniqueProducts

main = do
 print case1
 print case2
 print products
 print solution