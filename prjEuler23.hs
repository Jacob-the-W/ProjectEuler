divisorSum :: Integer -> Integer  -- don't care about order because we're summing.
divisorSum n =
  let limit = floor (sqrt (fromIntegral n))
  in if limit^2 == n
     then let pairs          = unzip $ [(m,div n m)|m<-[1..limit-1], mod n m==0]
              littleDivisors = fst pairs
              middle         = [limit]
              bigDivisors    = drop 1 $ snd pairs
          in sum (littleDivisors++middle++bigDivisors)
     else
       let pairs          = unzip $ [(m,div n m)|m<-[1..limit], mod n m==0]
           littleDivisors = fst pairs
           middle         = []
           bigDivisors    = drop 1 $ snd pairs
       in sum (littleDivisors++middle++bigDivisors)

isAbundant :: Integer -> Bool
isAbundant n = divisorSum n > n

abundantList :: Integer -> [Integer]
abundantList n = [x | x <- [1..n], isAbundant x]

isSumOfAbundants :: Integer -> Bool
isSumOfAbundants n = any (\x -> isAbundant (n-x)) (abundantList n)

sumNotSumOfAbundants :: Integer -> Integer
sumNotSumOfAbundants n = sum [x | x <- [1..n], not (isSumOfAbundants x)]

-- Call the sumNotSumOfAbundants function with a limit of 28123
main::IO()
main = do
  print $ sumNotSumOfAbundants 28123