module PrjEuler100 where
{-
Playing loose with limits,

a/(a+b) * (a-1)/(a+b-1) -> 1/2 
  implies 
  a/(a+b) -> 1/sqrt 2 = 1/(1 + sqrt 2 - 1), 

so a/b -> 1/(sqrt 2 - 1) = sqrt 2 + 1 

In particular a -> b*(sqrt 2 + 1) > b, so we'll iterate on b.

Why ceiling seems to work all around, and round/floor do not for 'a' is unclear to me. 

For a lower bound on b, note
a + b ~= b*(sqrt 2 + 1) + b 
       = b*(sqrt 2 + 2) > n whenever
                            b > n / (sqrt 2 + 2)

Starting b at ceiling (n / (sqrt 2 + 2)) makes sense here for sure.
-}

solution :: (Int, Int)
solution = head [(a,b)|b<-[ceiling (10^12/(sqrt 2 + 2))..], 
                       let a =  ceiling $ fromIntegral b*(sqrt 2 + 1),
                       2*a*(a-1) == (a+b)*(a+b-1)]

main :: IO ()
main = do 
    print solution
    putStrLn (show (fst solution) ++ " blue discs.")