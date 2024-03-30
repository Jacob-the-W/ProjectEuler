module PrjEuler44 where

pentagon :: Int -> Int
pentagon n = n*(3*n-1) `div` 2

pentagons :: [Int]
pentagons = map pentagon [1..]
--1, 5, 12, 22, 35, 51, 70, 92, 117, 145..

-- isPentagon n = n `elem` (takeWhile (<= n) pentagons) 
-- note n(3n-1)/2 has an inverse function in (1+sqrt(24*n+1))/6

isPentagon :: Int -> Bool
isPentagon n =
  let testValue = (1+sqrt (24*fromIntegral n+1))/6::Float
  in testValue == fromIntegral (floor testValue)

differencesOfPentagons :: [(Int,Int,Int)]
differencesOfPentagons = [(i,j,pentagon i - pentagon j)|i<-[1..],j<-[i-1,i-2..1]]

solution :: (Int,Int,Int)
solution = head $  filter (\(a,b,c) -> isPentagon (pentagon a + pentagon b)) $
             filter (\(a,b,c)->isPentagon c) differencesOfPentagons
--
main :: IO()
main = do
  let (a,b,c) = solution
  putStrLn ("Of the list of numbers which can be formed by taking differences of pentagonal numbers,\n"++
            show c ++" is the pentagonal number that results from P" ++show a++" - P"++show b++
            " = "++show (pentagon a) ++" - "++show (pentagon b) ++
            "\nWhere " ++show (pentagon a + pentagon b) ++ " is P"++show (floor (1+sqrt (24*fromIntegral (pentagon a + pentagon b)+1)/6))
            )