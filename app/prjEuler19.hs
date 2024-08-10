module PrjEuler19 where
--1 Jan 1900 was a Monday.

daysInMonth :: Int -> Int -> Int
daysInMonth y m
    | m == 2    = if y `mod` 400 == 0 || (y `mod` 4 == 0 && y `mod` 100 /= 0) then 29 else 28
    | m `elem` [4, 6, 9, 11] = 30
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | otherwise = error "Invalid month"
--monday = 1
dayOfWeek :: Int -> Int -> Int -> Int
dayOfWeek 1900 1 0 = 0
dayOfWeek y m d
  | y < 1900 = dayOfWeek (y + 400) m d
  | otherwise = (dayOfWeek 1900 1 0 + 
    sum [daysInMonth ys ms|ys<-[1900..y-1],ms<-[1..12]]
    + sum [daysInMonth y ms|ms<-[1..m-1]] + d) `mod` 7

solution :: Int
solution = length $ filter (==0)
  [dayOfWeek years months 1|years<-[1901..2000],months<-[1..12]]

main::IO()
main = do
  print solution

{--
extra cheaty solution?
 Take the expected number of days that happen on both the first and a Sunday:
 P(day =Sunday) =1/7, 
 P(day = 1st of month) = 12/365.2422
 Since those probabilites should be almost independent 
 we can approximate the probability that both happen as their 
 Expectation = 365.2422 days/year * 100 years from 1901-2000 * 1/7*12/365.2422 = 1200/7 = 171.428571..
 --}