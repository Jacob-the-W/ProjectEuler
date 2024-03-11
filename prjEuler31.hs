import Data.List
--bad, needs to be generalized, but I can't quite figure out how to take
--currencyValues and properly generate counters for each variable?
--might need to think on paper
--compiled runs 0.56s
waysToAddUpTo s = filter (\xs->last xs==s)
  [[c1,c2,c5,c10,c20,c50,c100,c200,1*c1+2*c2+5*c5+10*c10+20*c20+50*c50+100*c100+200*c200]|
     c1  <-[0..s `div` 1]  , 
     c2  <-[0..(s-c1) `div` 2] ,
     c5  <-[0..(s-c1-2*c2) `div` 5]  , 
     c10 <-[0..(s-c1-2*c2-5*c5) `div` 10],
     c20 <-[0..(s-c1-2*c2-5*c5-10*c10) `div` 20] , 
     c50 <-[0..(s-c1-2*c2-5*c5-10*c10-20*c20) `div` 50],
     c100<-[0..(s-c1-2*c2-5*c5-10*c10-20*c20) `div` 100], 
     c200<-[0..(s-c1-2*c2-5*c5-10*c10-20*c20) `div` 200]]
     
     
solution = length $ waysToAddUpTo 200

main :: IO()
main = do 
  putStrLn "A few examples:"
  print $ take 10 $ waysToAddUpTo 200
  putStrLn "The length of the full list is:"
  print $ length $ waysToAddUpTo 200