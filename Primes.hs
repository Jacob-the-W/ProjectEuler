module Primes'
  (composites,checkLoop,digits,divisors, fractionDigits, fractionDigitsBase,
  highlyComposites, isAbundant,isAmicable, isCube, isDeficient,
  isPerfect,isPrime,isPrime',isSquare,
  largelyComposites, main,multiplyPrimeFactors,
  numOfDistinctPrimeFactors,numOfDivisors,numOfPropDivisors,order,outer,
  primeFactorization,primeFactorization',primePowersListed,primeWithPowers,
  primes, primesToUA,propDivisors,sumDivisors,sumPropDivisors,
  sumTotient,sumTotients,totient,undigits) where

import Data.List
import Data.Ratio
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve

primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

primes = 2 : filter (\n -> all (\p -> n `rem` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..]


composites = setMinus [2..] primes where
  setMinus xs [] = xs
  setMinus [] _  = []
  setMinus (x:xs) (y:ys)
    |x == y  = setMinus xs ys
    |x < y   = x : setMinus xs (y:ys)
    |x > y   = setMinus (x:xs) ys

isPrime n = (n >= 2) && not (any (\p -> n `rem` p == 0) (takeWhile (\p -> p*p <= n) primes))

isPrime' n = (n >= 2) && not (any (\p -> n `rem` p == 0) (primesToUA $ ceiling (sqrt (fromIntegral n))))


primeFactorization n
  | n < 2     = []
  | even n = 2 : primeFactorization (n `quot` 2)
  | n `rem` 3 == 0 = 3 : primeFactorization (n `quot` 3)
  | n `rem` 5 == 0 = 5 : primeFactorization (n `quot` 5)
  | n `rem` 7 == 0 = 7 : primeFactorization (n `quot` 7)
  | otherwise =
      let divisors =  filter (\p -> (n `rem` p) == 0) $ takeWhile (\p -> p*p <=n) $ drop 4 primes
      in if null divisors
            then [n]
         else let divisor  = head divisors
              in  divisor : primeFactorization (n `quot` divisor)


primeFactorization' n
  | n < 2     = []
  | even n = 2 : primeFactorization (n `quot` 2)
  | n `rem` 3 == 0 = 3 : primeFactorization (n `quot` 3)
  | n `rem` 5 == 0 = 5 : primeFactorization (n `quot` 5)
  | n `rem` 7 == 0 = 7 : primeFactorization (n `quot` 7)
  | otherwise =
      let divisors =  filter (\p -> (n `rem` p) == 0) $ map fromIntegral $ primesToUA (fromIntegral $ ceiling (sqrt (fromIntegral n)))
      in if null divisors
            then let divisor = n
                 in  [divisor]
         else let divisor  = head divisors
              in  divisor : primeFactorization' (n `quot` divisor)

primeWithPowers n = map (\ps -> (head ps, length ps)) . group $ primeFactorization n

primePowersListed n =
  let partition = map (\(a,b) -> [a^k|k<-[0..b]]) (primeWithPowers n)
  in partition


--multiplyPrimeFactors xs = map (\ps -> (head ps, length ps)) .  group . sort . concat  $ map primeFactorization xs
multiplyPrimeFactors xs = f . sort . concatMap primeWithPowers $ xs where
  f [] = []
  f [(p,n)] = [(p,n)]
  f (x@(p,n):(y@(q,m):xs)) = if p == q then f ((p,m+n):xs) else (p,n):f (y:xs)

numOfDistinctPrimeFactors n = length $ primeWithPowers n

isSquare n =
 (n >= 0) && all (even . snd) (primeWithPowers n)

isCube n =
 (n >= 0) && all (((==0) . (`mod` 3)) . snd) (primeWithPowers n)

outer :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
outer f xs ys = [f x y| y<-ys,x<-xs]

divisors 1 = [1]
divisors n =  sort $ foldl1 (outer (*)) $ primePowersListed n

propDivisors n = if n<=1 then [] else filter (<n ) $ divisors n

sumDivisors n = if n == 0 then 0 else product $  map (\(a,b)-> (a^(b+1)-1) `div` (a-1)) $ primeWithPowers n

sumPropDivisors n = sumDivisors n - n

isAmicable  n =
  let m = sumPropDivisors n
  in (m /= n) &&  (sumPropDivisors m == n)
isPerfect   n = sumPropDivisors n                   == n
isAbundant  n = sumPropDivisors n                    > n
isDeficient n = sumPropDivisors n                    < n

numOfDivisors n = product [fromIntegral b+1|(_,b)<-primeWithPowers n]

numOfPropDivisors n = numOfDivisors n - 1

highlyComposites =
  let f [] = []; f ((a,b):xs) = (a,b):dropWhile (\(c,d)->c<=a) (f xs)
  in  map snd . f $ map (\n->(numOfDivisors n, n)) [1..]

largelyComposites =
  let f [] = []; f ((a,b):xs) = (a,b):dropWhile (\(c,d)->c<a) (f xs)
  in  map snd . f $ map (\n->(numOfDivisors n, n)) [1..]

totient n = product $ map (\(p,n)-> p^(n-1)*(p-1)) (primeWithPowers n)

sumTotient n = foldl' (+) 1 (map totient [1..n])
sumTotients = scanl' (+) 1 (map totient [1..])

order a b = head $ filter (\n -> (a^n) `rem` b == 1) (divisors $ totient b)

fractionDigits a b =
  let (p,q) = a `quotRem` b
  in p:fractionDigits (10*q) b

fractionDigitsBase n a b =
  let (p,q) = a `quotRem` b
      pdigits a =
        let (p',q') = a `quotRem` n
        in if p'==0
           then [q']
           else reverse $ q':pdigits p'
  in (pdigits p:fractionDigitsBase n (n*q) b )

digits a =
 let (p,q) = a `quotRem` 10
 in if p==0
    then [q]
    else q:digits p

undigits :: Integral a => [a]->a
undigits = foldr1 (\x y->x+10*y)

checkLoop [] = []
checkLoop (x : xs)
  | x>10^7-1 = []
  | null xs = [x]
  | otherwise = x:takeWhile (/=x) (checkLoop xs)


main :: IO ()
main = do
  putStrLn "Here's some examples of what this module can do:"
  putStrLn "The command primes generates the infinite list of primes."
  putStrLn "Careful with that and try takeWhile (<100) primes"
  putStr ">takeWhile (<100) primes"
  getLine
  print $ takeWhile (<100) primes

  putStrLn "\nSince the primes are defined, we also have composites as the numbers which aren't prime."
  putStr ">takeWhile (<100) composites"
  getLine
  print $ takeWhile (<100) composites

  putStr "\n>isPrime (2^31-1)"
  getLine
  print $ isPrime (2^31-1)

  putStr "\n>primeFactorization (2^3*3^5*5^7)"
  getLine
  print $ primeFactorization (2^3*3^5*5^7)

  putStr "\n>do print $ divisors 100; do print $ sumDivisors 100; do print $ propDivisors 100; do print $ sumPropDivisors 100; "
  getLine
  do print $ divisors 100; do print $ sumDivisors 100; do print $ propDivisors 100; do print $ sumPropDivisors 100;

  putStrLn "\nLargely composites are numbers with at least as many divisors as every number below them,"
  putStr ">take 25 largelyComposites"
  getLine
  print $ take 25 largelyComposites

  putStrLn "\nHighly composites are numbers with more divisors than every number below them,"
  putStr ">take 25 $ highlyComposites"
  getLine
  print $ take 25 highlyComposites

  putStrLn "\nWith sumPropDivisors defined, we can discuss abundant numbers, perfect numbers, deficient numbers, and amicabale pairs."
  putStrLn "A number n is abundant if sumPropDivisors n > n, perfect if sumPropDivisors n == n, deficient if ... < n."
  putStrLn "We use isAbundant, isAmicable, .."
  putStrLn $ "Abundants = \n" ++ show (filter  isAbundant [1..100])
  putStrLn $ "\nAmicable Pairs = \n" ++ show  (filter (uncurry (<))  [(a,sumPropDivisors a)|a<-[1..10000],isAmicable a])


  putStrLn "\nTwo other functions may be of interest, primeWithPowers and primePowersListed."
  putStrLn "primeWithPowers takes the prime factorization that is a list of repetition,"
  putStrLn "and converts each into it's respective prime power."
  putStr ">primeWithPowers (2^3*3^5*5^7)"
  getLine
  print $ primeWithPowers (2^3*3^5*5^7)


  putStr "\n>primePowersListed (2^3*3^5*5^7)"
  getLine
  print $ primePowersListed (2^3*3^5*5^7)

  putStrLn "\nTo get the divisors, we use a fold that generates all the products of each term in each list."
  putStr ">divisors (2^3*3^5*5^7)"
  getLine
  print $ divisors (2^3*3^5*5^7)

  putStrLn "\nWe could generate this list and calculate its length:"
  putStr ">length $ divisors (2^3*3^5*5^7)"
  getLine
  print $ length $ divisors (2^3*3^5*5^7)

  putStrLn "\nOne better way to count divisors is to note for each prime power, we have the choice from 0..n as possible factors."
  putStrLn "So we can simply add one to each exponent, and take the product of all those exponents."
  putStrLn "This is implemented in numOfDivisors, numOfPropDivisors."
  putStr ">do numOfDivisors (2^3*3^5*5^7); do numOfPropDivisors (2^3*3^5*5^7); do numOfDistinctPrimeFactors (2^3*3^5*5^7);"
  getLine
  do print $ numOfDivisors (2^3*3^5*5^7); do print $ numOfPropDivisors (2^3*3^5*5^7); do print $ numOfDistinctPrimeFactors (2^3*3^5*5^7);
  putStrLn "Clearly, 4*6*8=192."

  putStrLn "\nHere's a scanl version instead, on 2^4*3*5*7"
  putStr ">scanl1 (outer (*)) $ primePowersListed (2^4*3*5*7)"
  getLine
  print $ scanl1 (outer (*)) $ primePowersListed (2^4*3*5*7)

  putStrLn "\nWe can also compute the totient function on a number."
  putStrLn "The totient of a number n is the number of integers k so that 1<=k<=n for which"
  putStrLn "the gcd(n,k)==1. For example, totient 5 = 4, since gcd 5 5 = 5, and for all n<5, gcd 5 n == 1. "
  putStr ">map totient [1..20]"
  getLine
  print $ map totient [1..20]

  putStrLn "\nThe totient is related to the length of the Farey sequence of rationals between 0 and 1, i.e.,:"
  getLine
  putStrLn "0/1 1/1\n0/1 1/2 1/1\n0/1 1/3 1/2 2/3 1/1\n0/1 1/4 1/3 1/2 2/3 3/4 1/1\n0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1\n0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1\n0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1\n0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1\n0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1\n0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1\n"
  putStrLn "Compare the lengths of the above to the sum of the totients."
  putStr ">map sumTotient [1..10]"
  getLine
  print $ map sumTotient [1..10]
  putStr ">map (sumTotients!!) [1..10]"
  getLine
  print $ map (sumTotients!!) [1..10]
  putStr ">map (sumTotients!!) [1..100]"
  getLine
  print $ map (sumTotients!!) [1..100]



  putStrLn "\nIt can be shown that the order of a number a rem b, i.e.,"
  putStrLn "the least n satisifying a^n `rem` b == 1, must be a divisor of the the totient of b."
  putStr ">order 2 17"
  getLine
  print $ order 2 17
  putStrLn ("As in, 2^"++show (order 2 17)++" `rem` 17 == 1")

  putStrLn "\nExtra fun: consider"
  mapM_ (\m -> do
    putStr $ "\n>filter (\\n-> all(=="++show m++") $ map snd $ primeWithPowers n)[2..1024]"
    getLine
    print $ filter (all ((==m) . snd) . primeWithPowers) [2..1024]) [1..10]

  putStrLn "\nFinding biggest prime <= 10^12: first with a trial division sieve, then an array sieve:"
  putStrLn "do print . head $ filter isPrime  [10^12-1,10^12-3..]; do print . head $ filter isPrime' [10^12-1,10^12-3..];"
  do print . head $ filter isPrime  [10^12-1,10^12-3..]; do print . head $ filter isPrime' [10^12-1,10^12-3..];
