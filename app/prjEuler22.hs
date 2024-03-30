module PrjEuler22 where
{--
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

--}


import Data.Char ( ord )
import Data.List ( sort )

-- list::[(Char,Int)] -- list of [('A',1),('B',2),..]
-- list = map (\x -> (x,ord x - 64)) ['A'..'Z']

nameValue::String->Int
nameValue name = sum (map (\x -> ord x - 64) name)

score::(Int,String)->Int
score (i,name) = i*nameValue name

main::IO()
main = do
   input <- readFile "data\\p022_names.txt"
   let listOfStrings = sort $ read ("[" ++ input ++ "]")::[String]
   let indexedList = zip [1..] listOfStrings
   let scores = map score indexedList
--   putStrLn "Here's the indexed list:"--
--   print indexedList
--   putStrLn "Here's the scores:"
--   print scores
--   putStrLn "The sum of all scores is:"
   print $ sum scores