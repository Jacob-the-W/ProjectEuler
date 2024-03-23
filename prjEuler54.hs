module Main where
import Data.List ( group, nub, sort, sortBy )
import Data.Ord ( Down(Down), comparing )

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum)

instance Show Suit where
    show Clubs = "C"
    show Diamonds = "D"
    show Hearts = "H"
    show Spades = "S"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King | Ace
            deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show :: Rank -> String
  show r = case r of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "T"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)

instance Show Card where
    show (Card rank suit) = show rank ++ show suit

instance Ord Card where
    compare (Card rank1 _) (Card rank2 _) = compare rank1 rank2

-- Hand will be a synonym for a list of Cards, but it is expected that hand 
-- should be descending in card value to make the rest of the functions 
-- easier to reason about.
type Hand = [Card]

data HandRank = HighCard Hand | OnePair Rank | TwoPairs Rank Rank |
  ThreeOfAKind Rank | Straight Rank | Flush Rank | FullHouse Rank Rank |
  FourOfAKind Rank | StraightFlush Rank | RoyalFlush deriving (Eq, Ord)

instance Show HandRank where
    show (HighCard hand)         = "High Card:       "
    show (OnePair rank)          = "One Pair:        "
    show (TwoPairs rank1 rank2)  = "Two Pairs:       "
    show (ThreeOfAKind rank)     = "Three of a Kind: "
    show (Straight rank)         = "Straight:        "
    show (Flush rank)            = "Flush:           "
    show (FullHouse rank1 rank2) = "Full House:      "
    show (FourOfAKind rank)      = "Four of a Kind:  "
    show (StraightFlush rank)    = "Straight Flush:  "
    show RoyalFlush              = "Royal Flush:     "

readCard :: String -> Card
readCard [r,s] = Card { rank = readRank r, suit = readSuit s }
  where
    readRank '2' = Two
    readRank '3' = Three
    readRank '4' = Four
    readRank '5' = Five
    readRank '6' = Six
    readRank '7' = Seven
    readRank '8' = Eight
    readRank '9' = Nine
    readRank 'T' = Ten
    readRank 'J' = Jack
    readRank 'Q' = Queen
    readRank 'K' = King
    readRank 'A' = Ace
    readSuit 'C' = Clubs
    readSuit 'D' = Diamonds
    readSuit 'H' = Hearts
    readSuit 'S' = Spades

isFlush :: Hand -> Bool
isFlush = (==1) . length . nub . map suit

isStraight :: Hand -> Bool
isStraight hand =
  let hand' = reverse . map (fromEnum . rank) $ hand
  in all (==1) (zipWith (-) (tail hand') hand')

isStraightFlush :: Hand -> Bool
isStraightFlush hand = and ([isStraight, isFlush] <*> [hand])

isRoyalFlush :: Hand -> Bool
isRoyalFlush hand = isStraightFlush hand && (Ace == rank (head hand))

sizeOfGroups :: Hand -> [Int]
sizeOfGroups = sort . map length . group . map rank

isFourOfAKind :: Hand -> Bool
isFourOfAKind hand = [1,4] == sizeOfGroups hand

isFullHouse :: Hand -> Bool
isFullHouse hand = [2,3] == sizeOfGroups hand

isThreeOfAKind hand = [1,1,3] == sizeOfGroups hand

isTwoPair hand = [1,2,2] == sizeOfGroups hand

isOnePair hand = [1,1,1,2] == sizeOfGroups hand

handRank :: Hand -> HandRank
handRank hand
  | isRoyalFlush hand    = RoyalFlush
  | isStraightFlush hand = StraightFlush (rank . head $ hand)
  | isFourOfAKind hand   =
      FourOfAKind (head . map head . filter ((==4) . length) . group . map rank $ hand)
  | isFullHouse hand     =
    let go = sortBy (comparing length) . group . map rank $ hand
    in FullHouse (head . last $ go) (head . head $ go)
  | isFlush hand         = Flush (rank . head $ hand)
  | isStraight hand      = Straight (rank . head $ hand)
  | isThreeOfAKind hand  =
      ThreeOfAKind (head . map head . filter ((==3) . length) . group . map rank $ hand)
  | isTwoPair hand       =
      let go = concat . filter ((==2) . length) . group . map rank $ hand
      in TwoPairs (head go) (last go)
  | isOnePair hand       =
      let go = head . head . filter ((==2) . length) . group . map rank $ hand
      in  OnePair go
  | otherwise = HighCard hand

winner :: Hand -> Hand -> Int
winner hand1 hand2
    | rank1 > rank2 = 1
    | rank2 > rank1 = 2
    | hand1 > hand2 = 1
    | hand1 == hand2 = 0
    | otherwise = 2
  where
    rank1 = handRank hand1
    rank2 = handRank hand2

makeHand :: [Card] -> Hand  -- makes sure the list is sorted correctly for the rest of the functions to make sense.
makeHand = sortBy (comparing Down)

makeHands :: ([Card], [Card]) -> (Hand, Hand)
makeHands (a,b) = (makeHand a, makeHand b)


winnerString hand1 hand2 =
    concat [show (handRank hand1), show hand1, " | ",
            show (handRank hand2), show hand2,
            " -> Player ", show $ winner hand1 hand2, " wins."]

main::IO()
main = do
    inputGames <- readFile "p054_poker.txt"
    let games = map (makeHands . splitAt 5 . map readCard . words) (lines inputGames)
    mapM_ (putStrLn . uncurry winnerString) games
    let solution = length . filter ((==1) . uncurry winner) $ games
    print solution
