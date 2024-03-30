import Data.List (sort, sortBy, group)
import Primes (primes')
import Data.Ord ( Down(Down), comparing )

isPrime :: Int -> Bool
isPrime n =
  let r = floor . sqrt $ fromIntegral n
  in all (\p -> n `rem` p /=0) . takeWhile (<=r) $ primes'

runs :: Ord a => [a] -> [(Int, a)]
runs = sortBy (comparing Down) . map (\(g:gs) -> (length $ g:gs, g)) . group . sort

candidates :: [(Char, String)]
candidates = [(g, s) | p <- primes', let (s, g) = (show p, snd . head $ runs s)]

replace :: String -> Char -> Char -> String
replace ds old new = replace' <$> ds where
  replace' d = if d == old then new else d

expand :: (Char, String) -> [String]
expand (d, ds) = filter (isPrime . read) $ replace ds d <$> ['1'..'9']

main :: IO ()
main = do
  putStrLn $ snd . head $ filter ((==8) . length . expand) candidates