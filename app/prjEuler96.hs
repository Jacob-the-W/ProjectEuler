module PrjEuler96 (main) where

import Data.List ( minimumBy )
import Data.Array ( (!), (//), array, Array )
import Data.Char ( digitToInt )

type Cell = Int

makeCell :: Char -> Cell
makeCell c = if c `elem` ['0'..'9'] then digitToInt c:: Cell else 0::Cell

type Block = [Cell]

blocks :: Sudoku -> [Block]
blocks (Sudoku grid) = [[grid ! (i+x, j+y) | x <- [0..2], y <- [0..2]] | i <- [0,3,6], j <- [0,3,6]]

blockAt :: Int -> Int -> Sudoku -> Block
blockAt i j sudoku = blocks sudoku !! (bi*3 + bj)
  where (bi, bj) = (i `div` 3, j `div` 3)

row :: Int -> Sudoku -> Block
row i  (Sudoku grid) = [grid ! (i, j) | j <- [0..8]]

col ::  Int -> Sudoku -> Block
col j (Sudoku grid) = [grid ! (i, j) | i <- [0..8]]

type Grid = Array (Int, Int) Cell

makeGrid :: [String] -> Grid
makeGrid xs = array ((0,0), (8,8)) [((i,j), makeCell (xs !! i !! j)) | i <- [0..8], j <- [0..8]]

newtype Sudoku = Sudoku {getGrid :: Grid}

instance Show Sudoku where
  show (Sudoku grid) =
    unlines $ [(showCols i =<< [0 .. 8]) ++ rowPad i | i <- [0 .. 8]]
    where
      showCols i j = showCell (grid ! (i, j)) ++ showSeparator j
      showCell 0 = "_"
      showCell n = show n
      showSeparator j = if j `mod` 3 == 2 && j < 8 then " | " else " "
      rowPad i = if i `mod` 3 == 2 && i<8 then "\n------+-------+-------" else ""

makeSudoku :: [String] -> Sudoku
makeSudoku = Sudoku . makeGrid

data Problem = Problem {getStr ::String, getSudoku :: Sudoku}
makeProblem :: [String] -> Problem
makeProblem (x:xs) = Problem x (makeSudoku xs)
makeProblem _ = error "Invalid input"

solve :: Sudoku -> [Sudoku]
solve sudoku@(Sudoku grid)
  | null empties = [sudoku]  -- If there are no empty cells, the Sudoku is solved
  | otherwise = solve =<< newSudokus  -- Return all valid solutions
  where
    empties = findEmpty grid
    (i, j, possible) = findEasiest $ map (findPossible sudoku) empties
    newGrids = [replace (i, j) c grid | c <- possible]  -- Create new grids for each possible value
    newSudokus = map Sudoku newGrids  -- Create new Sudokus for each new grid

findPossible :: Sudoku -> (Int, Int) -> (Int, Int, [Cell])
findPossible sudoku (i, j) =
  let xRow = row i sudoku
      yCol = col j sudoku
      zBlock = blockAt i j sudoku
      emptyArray = array (0,9) [(k,0)|k<-[1..9]]
      cover = emptyArray // [(k,1)|k<-concat [xRow,yCol,zBlock]]
      possible =  [k|k<-[1..9], cover ! k == 0]  -- what's not in the cover
  in (i, j, possible)

findEmpty :: Grid -> [(Int, Int)]
findEmpty grid = [(i,j) | i <- [0..8], j <- [0..8], grid ! (i, j) == 0]

findEasiest :: [(Int, Int, [Cell])] -> (Int, Int, [Cell])
findEasiest (s@(_,_,possible):ss)
  | length possible == 1 = s
  | otherwise =
    minimumBy (\(_, _, pos) (_, _, pos')->
      compare (length pos) (length pos')) (s:ss)
findEasiest [] = undefined

replace :: (Int, Int) -> Cell -> Grid -> Grid
replace (i, j) c grid = grid // [((i, j), c)]

instance Show Problem where
    show (Problem a sudoku) = a ++ "\n" ++ show sudoku

splitGrids :: [a] -> [[a]]
splitGrids [] = []
splitGrids strings =
    let (a,b) = splitAt 10 strings
    in a:splitGrids b

prettyAnswer :: Show p => Problem -> p -> [String]
prettyAnswer Problem {getStr = n, getSudoku = start} end =
  n :zipWith (\x y -> concat [x, "   ", y]) (lines . show $ start) (lines . show $ end)

corner :: Sudoku -> Int
corner = foldl1 (\x y-> 10*x+y) . (\x->[getGrid x ! (0,j)|j<-[0..2]])

main :: IO()
main = do
    input <- lines <$> readFile "data\\p096_sudoku.txt"
    let problems = map makeProblem . splitGrids $ input
        solutions = (solve . getSudoku ) =<< problems
        (answers, corners) = unzip $ zipWith (\prob sol -> (prettyAnswer prob sol, corner sol)) problems solutions
        solution = sum corners
    mapM_ (putStrLn . unlines) answers
    print solution