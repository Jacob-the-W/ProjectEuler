module PrjEuler81 (main) where

main::IO()
main = do
  input <- readFile "data\\p081_matrix.txt"
  let inputArray = read input::[[Int]]
      solution = f inputArray 79 79
      f mat i j = mat!!i!!j + minimizer i j
      minimizer 0 0 = 0
      minimizer 0 y = head go !! (y-1)
      minimizer x 0 = head (go !! (x-1))
      minimizer x y = min (go !! x !! (y-1)) (go !! (x-1) !! y)
      go = [[f inputArray x y| y<-[0..]]|x<-[0..]]
  print solution


