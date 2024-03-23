{--If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
--}
import Data.List

isPythagorean :: (Num a, Eq a) => a -> a -> a -> Bool

isPythagorean a b c = a^2 + b^2 == c^2 --

{--
Note that
 a = min (a,b,c), a+a+a=3a<=a+b+c=p, we get a <= p/3.
 c = max (a,b,c), c+c+c=3c>=a+b+c=p, so c>= p `div` 3.

The latter is not as useful because it is two thirds of the range. 
            
Note pythagoreans exist of the form (a,b,b+1), so suppose a is very small.
 
Then a + b + b +1 ~= 2b +1 ~= p. Then b must go up to (p-1)div2 in the worst case.
More exactly, a + 2b + 1 = p for the worst case for b, so
b = (p-1-a)/2. 
--}
perimeters :: [Integer]
perimeters = [12..1000]

solutionsForPerimeter :: Integral a => a -> [(a, a, a)]
solutionsForPerimeter p = filter (\(a,b,c) -> isPythagorean a b c) 
                            [(a,b,p-a-b)|a<-[3..(p `div` 3)],b<-[a..(p-a-1) `div` 2]]

lengthOfSolutions p = length $ solutionsForPerimeter p 

solution = maximumBy (\(a,_) (b,_) -> compare a b) $ filter (\(a,_) -> a /= 0) [(lengthOfSolutions p,p)|p<-perimeters]

main::IO()
main = do
  print solution