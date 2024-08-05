module PrjEuler96 (main) where

import Data.Array.Unboxed
    ( (!), (//), array, assocs, listArray, UArray )
import Data.Char ( digitToInt, isDigit ) 

type Cell = Int

makeCell :: Char -> Cell
makeCell c = if isDigit c then digitToInt c:: Cell else 0::Cell

type Block = [Cell]

blockAt :: Int -> Int -> Sudoku -> Block
blockAt i j (Sudoku grid) =
  [grid ! (bi*3 + x, 3*bj + y) | x <- [0..2], y <- [0..2]]
  where (bi, bj) = (i `div` 3, j `div` 3)

row :: Int -> Sudoku -> Block
row i  (Sudoku grid) = [grid ! (i, j) | j <- [0..8]]

col ::  Int -> Sudoku -> Block
col j (Sudoku grid) = [grid ! (i, j) | i <- [0..8]]

type Grid = UArray (Int, Int) Cell

makeGrid :: [String] -> Grid
makeGrid xs = listArray ((0,0), (8,8)) (map makeCell =<< xs)

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
      emptyArray = array (0,9) [(k,0)|k<-[0..9]] :: UArray Int Int
      cover = emptyArray // [(k,1)|k<-concat [xRow,yCol,zBlock]]
      possible =  [k|k<-[1..9], cover ! k == 0]  -- what's not in the cover
  in (i, j, possible)

findEmpty :: Grid -> [(Int, Int)]
findEmpty grid = [(i,j)|((i, j), g) <- assocs grid, g == 0]

findEasiest :: [(Int, Int, [Cell])] -> (Int, Int, [Cell])
findEasiest (a@(_, _,posa):b@(_, _, posb):as)
  | m == 1 = a
  | m <= n = findEasiest (a:as)
  | otherwise = findEasiest (b:as)
  where (m, n) = (length posa, length posb)
findEasiest [a] = a
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
  n :zipWith combine (lines . show $ start) (lines . show $ end)
    where combine x y = concat [x, "   ", y]

corner :: Sudoku -> Int
corner = foldl1 (\x y-> 10*x+y) . (\x->[getGrid x ! (0,j)|j<-[0..2]])

main :: IO()
main = do
    input <- lines <$> readFile "data\\p096_sudoku.txt"
    let problems = map makeProblem . splitGrids $ input
        solutions = (solve . getSudoku ) =<< problems
        (answers, corners) = unzip $ zipWith combine problems solutions
          where combine prob sol = (prettyAnswer prob sol, corner sol)
        solution = sum corners
    mapM_ (putStrLn . unlines) answers
    print solution