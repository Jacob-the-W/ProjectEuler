isPythagorean a b c = a^2 + b^2 == c^2

solutionsForPerimeter p = filter (\[a,b,c] -> isPythagorean a b c) 
                            [[a,b,p-a-b]|a<-[3..(p `div` 3)],b<-[a..(p-a-1) `div` 2]]
                            
main :: IO()
main = do print $ product $ head $ solutionsForPerimeter 1000                           