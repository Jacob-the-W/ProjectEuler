{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Ratio
import Data.Time.Clock.System ( getSystemTime, systemSeconds, systemNanoseconds, SystemTime (systemNanoseconds) )

import Primes (prettyPrint)
import PrjEuler1 (main)
import PrjEuler2 (main)
import PrjEuler3 (main)
import PrjEuler4 (main)
import PrjEuler5 (main)
import PrjEuler6 (main)
import PrjEuler7 (main)
import PrjEuler8 (main)
import PrjEuler9 (main)
import PrjEuler10 (main)
import PrjEuler11 (main)
import PrjEuler12 (main)
import PrjEuler13 (main)
import PrjEuler14 (main)
import PrjEuler15 (main)
import PrjEuler16 (main)
import PrjEuler17 (main)
import PrjEuler18 (main)
import PrjEuler19 (main)
import PrjEuler20 (main)
import PrjEuler21 (main)
import PrjEuler22 (main)
import PrjEuler23 (main)
import PrjEuler24 (main)
import PrjEuler25 (main)
import PrjEuler26 (main)
import PrjEuler27 (main)
import PrjEuler28 (main)
import PrjEuler29 (main)
import PrjEuler30 (main)
import PrjEuler31 (main)
import PrjEuler32 (main)
import PrjEuler33 (main)
import PrjEuler34 (main)
import PrjEuler35 (main)
import PrjEuler36 (main)
import PrjEuler37 (main)
import PrjEuler38 (main)
import PrjEuler39 (main)
import PrjEuler40 (main)
import PrjEuler41 (main)
import PrjEuler42 (main)
import PrjEuler43 (main)
import PrjEuler44 (main)
import PrjEuler45 (main)
import PrjEuler46 (main)
import PrjEuler47 (main)
import PrjEuler48 (main)
import PrjEuler49 (main)
import PrjEuler50 (main)
import PrjEuler51 (main)
import PrjEuler52 (main)
import PrjEuler53 (main)
import PrjEuler54 (main)
import PrjEuler55 (main)
import PrjEuler56 (main)
import PrjEuler57 (main)
import PrjEuler58 (main)
import PrjEuler59 (main)
import PrjEuler60 (main)
import PrjEuler62 (main)
import PrjEuler63 (main)
import PrjEuler65 (main)
import PrjEuler66 (main)
import PrjEuler67 (main)
import PrjEuler69 (main)
import PrjEuler70 (main)
import PrjEuler71 (main)
import PrjEuler75 (main)
import PrjEuler95 (main)
import PrjEuler96 (main)
import PrjEuler100 (main)
import Data.List
import Text.Read (readMaybe)

isIndex :: Int -> Bool
isIndex = (`Map.member` solutions)

solutions :: Map Int (IO ())
solutions = Map.fromDistinctAscList
  [(-1, runAll),
  (1, PrjEuler1.main),  (2, PrjEuler2.main),  (3, PrjEuler3.main),
  (4, PrjEuler4.main),  (5, PrjEuler5.main),  (6, PrjEuler6.main),
  (7, PrjEuler7.main),  (8, PrjEuler8.main),  (9, PrjEuler9.main),
  (10, PrjEuler10.main),(11, PrjEuler11.main),(12, PrjEuler12.main),
  (13, PrjEuler13.main),(14, PrjEuler14.main),(15, PrjEuler15.main),
  (16, PrjEuler16.main),(17, PrjEuler17.main),(18, PrjEuler18.main),
  (19, PrjEuler19.main),(20, PrjEuler20.main),(21, PrjEuler21.main),
  (22, PrjEuler22.main),(23, PrjEuler23.main),(24, PrjEuler24.main),
  (25, PrjEuler25.main),(26, PrjEuler26.main),(27, PrjEuler27.main),
  (28, PrjEuler28.main),(29, PrjEuler29.main),(30, PrjEuler30.main),
  (31, PrjEuler31.main),(32, PrjEuler32.main),(33, PrjEuler33.main),
  (34, PrjEuler34.main),(35, PrjEuler35.main),(36, PrjEuler36.main),
  (37, PrjEuler37.main),(38, PrjEuler38.main),(39, PrjEuler39.main),
  (40, PrjEuler40.main),(41, PrjEuler41.main),(42, PrjEuler42.main),
  (43, PrjEuler43.main),(44, PrjEuler44.main),(45, PrjEuler45.main),
  (46, PrjEuler46.main),(47, PrjEuler47.main),(48, PrjEuler48.main),
  (49, PrjEuler49.main),(50, PrjEuler50.main),(51, PrjEuler51.main),
  (52, PrjEuler52.main),(53, PrjEuler53.main),(54, PrjEuler54.main),
  (55, PrjEuler55.main),(56, PrjEuler56.main),(57, PrjEuler57.main),
  (58, PrjEuler58.main),(59, PrjEuler59.main),(60, PrjEuler60.main),
  (62, PrjEuler62.main),(63, PrjEuler63.main),(65, PrjEuler65.main),
  (66, PrjEuler66.main),(67, PrjEuler67.main),(69, PrjEuler69.main),
  (70, PrjEuler70.main),(71, PrjEuler71.main),(75, PrjEuler75.main), 
  (95, PrjEuler95.main),(96, PrjEuler96.main),(100,PrjEuler100.main)]

runAll :: IO ()
runAll = do problemsPrint . Map.toList $ Map.drop 1 solutions; do putStr "\nTotal"

problemsPrint :: [(Int, IO ())] -> IO ()
problemsPrint =
    mapM_ (\(i,action) -> do
      putStrLn ("Problem " ++ show i ++ ":\n")
      x <- getSystemTime

      let (!s1,!ns1) = (systemSeconds x, systemNanoseconds x)
      do action
      y <- getSystemTime
      let (!s2, !ns2)= (systemSeconds y, systemNanoseconds y)
          !diff = fromIntegral s2 - fromIntegral s1 +
            fromIntegral ns2%1000000000 -
            fromIntegral ns1%1000000000 :: Rational
          !diffS = prettyPrint diff 9
          !diff_ms = prettyPrint (1000*diff) 6
          !diff_us = prettyPrint (1000000*diff) 3
          !diff_ns = show . round $ (1000000000*diff)
      putStrLn ("\nTime: " ++ diffS ++ " seconds.")
      putStrLn ("      " ++ diff_ms ++ " milliseconds.")
      putStrLn ("      " ++ diff_us ++ " microseconds.")
      putStrLn ("      " ++ diff_ns ++ " nanoseconds.\n")
      appendFile "logs.txt" $ show i ++ " " ++ diff_ns ++ "\n")

cleanUpLogs :: FilePath -> IO ()
cleanUpLogs logFile = do
  input <- lines <$> readFile logFile
  let !pts = split . words <$> input where
         split (p:(t:_)) = (p, t)
         split _ = ("-1", "error")
      !problems = fst <$> pts
      !n_p = maximum $ length <$> problems
      !problems' = pad <$> problems where
        pad p = replicate (n_p - length p) ' ' ++ p
      !times    = snd <$> pts
      !n_t = maximum $ length <$> times
      !times'   = pad <$> times where
        pad t = replicate (n_t - length t) ' ' ++ t
  writeFile logFile "Problem Time(ns)\n"
  mapM_ (appendFile logFile) $ zipWith (\p t -> p ++ " " ++ t ++ "\n") problems' times'

indexMessage :: [Int] -> [Int] -> String
indexMessage indices badIndices
  | -1 `elem` indices = "\nRan problems " ++ intercalate ", " (map show (drop 1 $ Map.keys solutions)) ++ "."
  | otherwise = "\n" ++ case (indices, badIndices) of
  ([] , [] ) -> "No problems entered at all?"
  ([] , [_]) -> "Problem " ++ sBad ++ " not found."
  ([] , _  ) -> "Problems " ++ sBad ++ " not found."
  ([_], [] ) -> "Ran problem " ++ sGood ++ "."
  ([_], [_]) -> "Ran problem " ++ sGood ++ ", but problem " ++ sBad ++ " not found."
  ([_], _  ) -> "Ran problem " ++ sGood ++ ", but problems " ++ sBad ++ " not found."
  (_  , [] ) -> "Ran problems " ++ sGood ++ "."
  (_ , [_] ) -> "Ran problems " ++ sGood ++ ", but problem " ++ sBad ++ " not found."
  (_  , _  ) -> "Ran problems " ++ sGood ++ ", but problems " ++ sBad ++ " not found."
  where
    sBad = intercalate ", " (show <$> badIndices)
    sGood = intercalate ", " (show <$> indices)

logMessage :: [Int] -> String
logMessage indices =
  (if null indices then "Because no valid problems were chosen, logs will not be modified.\n" ++
    "If logs.txt does not exist, it will not be created.\n\n"
  else "Log file: logs.txt\n\n")  ++ replicate 80 '-' ++ "\n"

main :: IO ()
main = do
  putStrLn "-- Project Euler solutions: Haskell --\n"

  putStrLn "To exit, enter a blank line, or any non-numeric input."
  putStrLn "To run a solution, enter a number from the following list:"
  print $ Map.keys solutions
  putStrLn "^ to run the corresponding Project Euler problem.\n"
  putStrLn "Enter -1 to run all problems (warning: long output)"
  putStrLn
     "Enter a space separated list of numbers to run multiple problems."

  input <- getLine

  let (indices, badIndices) = partition isIndex . map head . group . sort . 
        mapMaybe (\x -> readMaybe x :: Maybe Int) $ words input
      (nI, nBI) = (null indices, null badIndices)

  unless nI $ writeFile "logs.txt" ""

  mapM_ (\index ->  problemsPrint [(index, solutions Map.! index)]) indices
  putStrLn $ indexMessage indices badIndices

  unless nI (putStrLn "\nCreating logs...")
  unless nI (cleanUpLogs "logs.txt")
  unless nI (putStrLn "Logs created, cleaned up.")
  putStrLn $ logMessage indices

  let exitConditions = -1 `elem` indices || (nI && nBI)

  when exitConditions (putStrLn "Exiting.")
  unless exitConditions Main.main