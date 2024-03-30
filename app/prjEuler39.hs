module PrjEuler39 where

{--If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
--}

isPythagorean :: Int -> Int -> Int -> Bool
isPythagorean a b c = a^2 + b^2 == c^2 

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
perimeters :: [Int]
perimeters = [12..1000]

solutionsForPerimeter :: Int -> [(Int,Int,Int)]
solutionsForPerimeter p = 
  [(a,b,c)| a<-[3..(p `div` 3)],
    b<-[a+1..(p-a-1) `div` 2],
    let c = p - a - b, isPythagorean a b c]

lengthOfSolutions :: Int -> Int
lengthOfSolutions = length . solutionsForPerimeter 

solution :: Int
solution = snd . maximum . flip zip perimeters $ lengthOfSolutions <$> perimeters

main::IO()
main = do
  print solution