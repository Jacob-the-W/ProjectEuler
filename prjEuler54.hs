{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
import Data.List

valuesFromHand :: [[a]] -> [[a]]
valuesFromHand = map (take 1)

suitsFromHand :: [[a]] -> [[a]]
suitsFromHand = map (drop 1 )

handRanks :: [String]
handRanks = ["Highest Cards","One Pair","Two Pairs",
  "Three of a Kind", "Straight", "Flush", "Full House",
  "Four of a Kind", "Straight Flush", "Royal Flush"]

cardRank :: Num a => String -> a
cardRank "2" = 2
cardRank "3" = 3
cardRank "4" = 4
cardRank "5" = 5
cardRank "6" = 6
cardRank "7" = 7
cardRank "8" = 8
cardRank "9" = 9
cardRank "T" = 10
cardRank "J" = 11
cardRank "Q" = 12
cardRank "K" = 13
cardRank "A" = 14
cardRank _ = 0

cardRanks :: (Ord a, Num a) => [[Char]] -> [a]
cardRanks hand = sort $ map cardRank $ valuesFromHand hand

playerHandRank:: (Ord a1, Num a2, Num a1) => [[Char]] -> (a2, [a1])
playerHandRank hand
 |isRoyalFlush hand = (9,cardRanks hand)
 |isStraightFlush hand = (8,cardRanks hand)
 |isFourOfAKind hand = (7, cardRanks hand)
 |isFullHouse hand = (6, cardRanks hand)
 |isFlush hand = (5, cardRanks hand)
 |isStraight hand = (4, cardRanks hand)
 |isThreeOfAKind hand = (3, cardRanks hand)
 |isTwoPair hand = (2, cardRanks hand)
 |isOnePair hand = (1, cardRanks hand)
 |otherwise  = (0, cardRanks hand)

playerHandRank' :: (Ord a, Num a) => [[Char]] -> (String, [a])
playerHandRank' hand = (\(a,b)->(handRanks!!a,b)) $ playerHandRank hand

player :: (Eq a1, Num a1) => a1 -> (a2, a2) -> a2
player 1 gameRound = fst gameRound -- is a "hand"
player 2 gameRound = snd gameRound

isStraight :: [[Char]] -> Bool
isStraight hand =
  let hand' = cardRanks hand
  in zipWith (-) (tail hand') hand' == replicate 4 1
  --in if [1,1,1,1] == (zipWith (-) (tail hand') hand ) then True else False

isFourOfAKind :: [[Char]] -> Bool
isFourOfAKind hand = any ((>=4) . length) (group $ sort $ valuesFromHand hand)

isFullHouse :: [[Char]] -> Bool
isFullHouse hand =
 let hand' =  map length $ group $ sort $ valuesFromHand hand
 in hand' == [2,3] || hand' == [3,2]

isFlush :: [[Char]] -> Bool
isFlush hand =
 let numOfGroups = length $ map length $ group $ sort $ suitsFromHand hand
 in numOfGroups == 1

isStraightFlush :: [[Char]] -> Bool
isStraightFlush hand = isStraight hand && isFlush hand

isRoyalFlush :: [[Char]] -> Bool
isRoyalFlush hand = isStraightFlush hand &&
  ("TJQKA"== concat (sortBy (\x y -> compare (cardRank x) (cardRank y)) $ valuesFromHand hand))

sizeOfGroups :: Ord a => [[a]] -> [Int]
sizeOfGroups hand = sort $ map length $ group $ sort $ valuesFromHand hand

isThreeOfAKind hand = [1,1,3] == sizeOfGroups hand

isTwoPair hand = [1,2,2] == sizeOfGroups hand

isOnePair hand = [1,1,1,2] == sizeOfGroups hand

highCardsCheck (hand1 ,hand2) =
 let values1 = reverse $ cardRanks hand1
     values2 = reverse $ cardRanks hand2
     f (x:xs) (y:ys)
       | x>y = 1
       | x<y = 2
       | otherwise = f xs ys
 in f values1 values2

winnerForFullHouse (hand1, hand2) =
 let fHCheck  x = head $ head $ nub $ filter (\x -> length x == 3) $ group $ sort $ cardRanks x
     highCard1 = fHCheck hand1
     highCard2 = fHCheck hand2
 in if highCard1 > highCard2 then 1
    else if highCard2 > highCard1 then 2
    else highCardsCheck (hand1, hand2)

winnerForFourOfAKind (hand1, hand2) =
 let fourCheck  x = head $ head $ nub $ filter (\x -> length x == 4) $ group $ sort $ cardRanks x
     highCard1 = fourCheck hand1
     highCard2 = fourCheck hand2
 in if highCard1 > highCard2 then 1
    else if highCard2 > highCard1 then 2
                   else highCardsCheck (hand1, hand2)

winnerForThreeOfAKind (hand1, hand2) =
 let threeCheck  x = head $ head $ nub $ filter (\x -> length x == 3) $ group $ sort $ cardRanks x
     highCard1 = threeCheck hand1
     highCard2 = threeCheck hand2
 in if highCard1 > highCard2 then 1
    else if highCard2 > highCard1 then 2
    else highCardsCheck (hand1, hand2)

winnerForTwoPair (hand1, hand2) =
  let twoPairCheck  x = nub . concat $ filter (\x -> length x == 2) $ group . sort $ cardRanks x
      highCard1 = maximum (twoPairCheck hand1)
      highCard2 = maximum (twoPairCheck hand2)
  in if highCard1 > highCard2 then 1
    else if highCard2 > highCard1 then 2
    else highCardsCheck (hand1, hand2)

winnerForOnePair (hand1, hand2) =
  let pairCheck x =  head $ head $ filter (\x -> length x == 2) $ group . sort $ cardRanks x
      highCard1 = pairCheck hand1
      highCard2 = pairCheck hand2
  in if highCard1 > highCard2 then 1
    else if highCard2 > highCard1 then 2
    else highCardsCheck (hand1, hand2)

winner (hand1,hand2) =
 let rank1 = fst (playerHandRank hand1)
     rank2 = fst (playerHandRank hand2)
 in if rank1 > rank2 then 1
    else if rank2 > rank1 then 2
    else if rank1 == 6 then winnerForFullHouse (hand1, hand2)
    else if rank1 == 7 then winnerForFourOfAKind (hand1, hand2)
    else if rank1 == 3 then winnerForThreeOfAKind (hand1, hand2)
    else if rank1 == 2 then winnerForTwoPair (hand1, hand2)
    else if rank1 == 1 then winnerForOnePair (hand1, hand2)
    else highCardsCheck (hand1, hand2)

main::IO()
main = do
    inputGames <- readFile "p054_poker.txt"
    let games = map (splitAt 5 . words) (lines inputGames)
    -- turns the txt file into a list of tuples where (x,y) has x as player 1s hand, y as player 2s hand
    mapM_ (\n -> do
      print (playerHandRank' (player 1 $ games!!n),playerHandRank' (player 2 $ games!!n),"Player "++ show (winner (games!!n))++" wins.")) [0..15]
    let solution = length $ filter (==1) $ map winner games
    print solution