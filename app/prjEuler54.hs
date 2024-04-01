
module PrjEuler54 where
import Data.List ( group, sortBy )
import Data.Ord ( comparing )
import Text.Read

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum)

instance Read Suit where
  readsPrec _ [s] = case s of
    'C' -> [(Clubs, "")]
    'D' -> [(Diamonds, "")]
    'H' -> [(Hearts, "")]
    'S' -> [(Spades, "")]
    _ -> []
  readsPrec _ _ = []    

instance Show Suit where
    show s = ["CDHS" !! fromEnum s]

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King | Ace
            deriving (Eq, Ord, Enum, Bounded)

instance Read Rank where
  readsPrec _ [r] = case r of
    '2' -> [(Two, "")];   '3' -> [(Three, "")]
    '4' -> [(Four, "")];  '5' -> [(Five, "")]
    '6' -> [(Six, "")];   '7' -> [(Seven, "")]
    '8' -> [(Eight, "")]; '9' -> [(Nine, "")]
    'T' -> [(Ten, "")];   'J' -> [(Jack, "")]
    'Q' -> [(Queen, "")]; 'K' -> [(King, "")]
    'A' -> [(Ace, "")]
    _ -> []
  readsPrec _ _ = []

instance Show Rank where
  show r =   [(['2'..'9'] ++ "TJQKA") !! fromEnum r]

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)

instance Read Card where
  readsPrec _ (r:s:_) = case (readMaybe [r] :: Maybe Rank, readMaybe [s] :: Maybe Suit) of
      (Just r', Just s') -> [(Card r' s', "")]
      (Nothing, Just _) -> error $ "Invalid rank: " ++ [r]
      (Just _, Nothing) -> error $ "Invalid suit: " ++ [s]
      (Nothing, Nothing) -> error $ "Invalid card: " ++ [r,s]
  readsPrec _ "" = error "Empty card"
  readsPrec _ [r] = error $ "Empty suit: maybe the rank is " ++ [r] ++ "?"

instance Show Card where
    show (Card { rank = r , suit = s }) = show r ++ show s

instance Ord Card where
    compare Card {rank = r1} Card {rank = r2} = compare r1 r2

-- Hand will be a synonym for a list of Cards, but it is expected that hand 
-- should be descending in card value to make the rest of the functions 
-- easier to reason about.
type Hand = [Card]

data HandRank = HighCard Hand | OnePair Rank | TwoPairs Rank Rank |
  ThreeOfAKind Rank | Straight Rank | Flush Rank | FullHouse Rank Rank |
  FourOfAKind Rank | StraightFlush Rank | RoyalFlush deriving (Eq, Ord)

instance Show HandRank where
 show hr = case hr of
  HighCard _         -> "High Card:       "
  OnePair _          -> "One Pair:        "
  TwoPairs _ _  -> "Two Pairs:       "
  ThreeOfAKind _     -> "Three of a Kind: "
  Straight _         -> "Straight:        "
  Flush _            -> "Flush:           "
  FullHouse _ _ -> "Full House:      "
  FourOfAKind _      -> "Four of a Kind:  "
  StraightFlush _    -> "Straight Flush:  "
  RoyalFlush            -> "Royal Flush:     "


groups :: Hand -> [(Int, Rank)]
groups hand = map groupLengths . sortBy (comparing length) . group $ rank <$> hand where
  groupLengths [] = error "Empty hand"
  groupLengths (g:gs) = (1+ length gs, g)

handRank :: Hand -> HandRank
handRank hand
  | isRoyalFlush       = RoyalFlush
  | isStraightFlush    = StraightFlush lead
  | [1,4] == sizes     = FourOfAKind ( gs !! 1)
  | [2,3] == sizes     = FullHouse (gs !! 1) (head gs)
  | isFlush            = Flush lead
  | isStraight         = Straight lead
  | [1,1,3] == sizes   = ThreeOfAKind (gs !! 2)
  | [1,2,2] == sizes   = TwoPairs (gs !! 2) (gs !! 1)
  | [1,1,1,2] == sizes = OnePair (gs !! 3)
  | otherwise          = HighCard hand where
   lead = rank . head $ hand
   (sizes, gs) = unzip . groups $ hand -- 
   isRoyalFlush = isStraightFlush && (Ace == lead)
   isStraightFlush = isStraight && isFlush
   isStraight =
     let hand' = fromEnum . rank <$> hand
     in all (==1) (zipWith (-) hand' (tail hand'))
   isFlush = sweep $  suit <$> hand where
      sweep (s:ss) = all (== s) ss
      sweep _ = undefined

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
makeHand = sortBy (flip compare)

makeHands :: ([Card], [Card]) -> (Hand, Hand)
makeHands (a,b) = (makeHand a, makeHand b)

winnerString :: Hand -> Hand -> String
winnerString hand1 hand2 =
    concat [show (handRank hand1), show hand1, " | ",
            show (handRank hand2), show hand2,
            " -> Player ", show $ winner hand1 hand2, " wins."]

solution :: [(Hand, Hand)] -> Int
solution = length . filter ((==1) . uncurry winner)

main::IO()
main = do
    inputGames <- readFile "data\\p054_poker.txt"
    let games = makeHands . splitAt 5 . map read . words <$> lines inputGames
    mapM_ putStrLn $ uncurry winnerString <$> take 10 games
    print . solution $ games