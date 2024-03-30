{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PrjEuler51 where

import Data.List (sort, sortBy, group)
import Primes (primes, isPrime)
import Data.Ord ( Down(Down), comparing )



runs :: [Char] -> [(Int, Char)]
runs = sortBy (comparing Down) . map (\(g:gs) -> (length $ g:gs, g)) . group . sort

candidates :: [(Char, String)]
candidates = [(g, s) | p <- primes :: [Int], let (s, g) = (show p, snd . head $ runs s)]

replace :: String -> Char -> Char -> String
replace ds old new = replace' <$> ds where
  replace' d = if d == old then new else d

expand :: (Char, String) -> [String]
expand (d, ds) = filter (isPrime . (\x -> read x :: Int)) $ replace ds d <$> ['1'..'9']

main :: IO ()
main = do
  putStrLn $ snd . head $ filter ((==8) . length . expand) candidates