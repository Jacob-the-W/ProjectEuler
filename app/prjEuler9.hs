module PrjEuler9 where

isPythagorean a b c = a^2 + b^2 == c^2

solutionsForPerimeter p = 
    [[a,b,c]|a<-[3..(p `div` 3)],b<-[a..(p-a-1) `div` 2],
      let c = p-a-b, isPythagorean a b c]
                            
main :: IO()
main = do print $ product $ head $ solutionsForPerimeter 1000                           