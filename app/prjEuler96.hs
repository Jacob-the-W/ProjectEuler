module PrjEuler96 where
  
import Data.List ((\\), sortBy, foldl')
import Data.Ord (comparing)
import Data.Array ( Array, array, (!), (//) )

type Cell = Int

makeCell :: Char -> Cell
makeCell c = if c `elem` ['0'..'9'] then read [c]:: Cell else 0::Cell

type Block = [Cell]

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = [[rows ! (i+x, j+y) | x <- [0..2], y <- [0..2]] | i <- [0,3,6], j <- [0,3,6]]

blockAt :: Int -> Int -> Sudoku -> Block
blockAt i j sudoku = blocks sudoku !! (bi*3 + bj)
  where (bi, bj) = (i `div` 3, j `div` 3)

row :: Int -> Sudoku -> Block
row i  (Sudoku rows) = [rows ! (i, j) | j <- [0..8]]

col ::  Int -> Sudoku -> Block
col j (Sudoku rows) = [rows ! (i, j) | i <- [0..8]]

type Grid = Array (Int, Int) Cell

makeGrid :: [String] -> Grid
makeGrid xs = array ((0,0), (8,8)) [((i,j), makeCell (xs !! i !! j)) | i <- [0..8], j <- [0..8]]

newtype Sudoku = Sudoku { sRows :: Grid }

instance Show Sudoku where
    show (Sudoku grid) = unlines $ [concatMap (\j ->
           (let g = grid ! (i, j) in if g == 0 then "_" else show g) ++ if j `mod` 3 == 2 && j<8 then " | " else " ") [0 .. 8]
                               ++ if i `mod` 3 == 2 && i<8 then "\n------+-------+-------" else "" | i <- [0 .. 8]]

makeSudoku :: [String] -> Sudoku
makeSudoku = Sudoku . makeGrid

data Problem = Problem {str::String, problem:: Sudoku}

makeProblem :: [String] -> Problem
makeProblem (x:xs) = Problem x (makeSudoku xs)
makeProblem _ = error "Invalid input"

solve :: Sudoku -> [Sudoku]
solve sudoku@(Sudoku grid)
  | null empties = [sudoku]  -- If there are no empty cells, the Sudoku is solved
  | otherwise = solve =<< newSudokus  -- Return all valid solutions
  where
    empties = findEmpty grid
    (i, j, possible) = head $ findEasiest $ map (findPossible sudoku) empties
    newGrids = [replace (i, j) c grid | c <- possible]  -- Create new grids for each possible value
    newSudokus = map Sudoku newGrids  -- Create new Sudokus for each new grid

findPossible :: Sudoku -> (Int, Int) -> (Int, Int, [Cell])
findPossible sudoku (i, j) =
  let xRow = row i sudoku
      yCol = col j sudoku
      zBlock = blockAt i j sudoku
      possible = foldl' (\\) [1..9] [xRow, yCol, zBlock]
  in (i, j, possible)

findEmpty :: Grid -> [(Int, Int)]
findEmpty grid = [(i,j) | i <- [0..8], j <- [0..8], grid ! (i, j) == 0]

findEasiest :: [(Int, Int, [Cell])] -> [(Int, Int, [Cell])]
findEasiest = sortBy (comparing (\(_, _, possible) -> length possible))

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
prettyAnswer (Problem n start) end =
  n:zipWith (\x y -> x ++ "   " ++ y) (lines . show $ start) (lines . show $ end)

main :: IO()
main = do
    input <- lines <$> readFile "data\\p096_sudoku.txt"
    let problems = map makeProblem . splitGrids $ input
        solutions = (solve . problem ) =<< problems
        solution = sum . map (foldl1 (\x y-> 10*x+y) . (\x->[sRows x ! (0,j)|j<-[0..2]])) $ solutions
    mapM_ (\(prob, sol)->do 
      (putStrLn . unlines) (prettyAnswer prob sol)) $ zip problems solutions
    print solution