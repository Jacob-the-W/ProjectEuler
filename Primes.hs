-- |  Module      : <File name or $Header$ to be replaced automatically>
--    Description : A collection of functions related to prime numbers.
--    Maintainer  : 
--    Stability   : experimental
--
--    The Primes module also re-exports the following function:
--    '%': From Data.Ratio
module Primes
  ( -- * Prime numbers
    -- **Numbers
    primes, composites, compositesTo, primes', primesToUA,
    -- **Tests
     isPrime, isPrime',
    -- **Functions
    primePowers,  primePowersListed, primeFactors, primeFactors', primePowers',
    multiplyPrimeFactors, numOfDistinctPrimeFactors,  totient, charmichael,

    -- * Divisors
    divisors, numOfDivisors, propDivisors, numOfPropDivisors, highlyComposites, largelyComposites,
    sumDivisors, sumPropDivisors, isAmicable, isPerfect, isDeficient, isAbundant, amicablePairs, abundantsTo,

    -- * Other number properties
    isSquare, isCube, isPower, order, gcdext, mu,

    -- * Fractions and ratios
    farey, sumTotient, sumTotients, fractionDigits,
    nthDigit, fractionDigitsBase, prettyPrint,


    -- * Digit manipulation
    digits, digitsBase, undigits, modPow,

    -- * Utility functions
    checkLoop, merge, setMinus, outer,

    -- * Re-exports
    (%) -- ^ From 'Data.Ratio'. Creates a 'Ratio' (fraction) from two 'Integral' numbers. For example, `(5 % 3)` creates the fraction 5/3.
  ) where

import Data.List ( group, sort, scanl', (\\))
import Data.Ratio ( (%), denominator, numerator )
import Control.Monad ( forM_, when )
import Data.Array
import Data.Array.ST
    ( readArray, writeArray, runSTUArray, MArray(newArray) )
import Data.Array.Unboxed ( UArray, assocs )
import Data.IntMap qualified as Map
import qualified Data.Set as Set
import Data.IntSet qualified as IntSet

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

-- | 
-- Taken directly from the wiki, uses Unboxed Arrays to perform a traditional imperative type sieve up to the input.
-- [More details](https://wiki.haskell.org/Prime_numbers#Using_ST_Array)
--
-- @
-- > length $ primesToUA (10^8)
-- 5761455
-- (0.31 secs, 467,223,208 bytes)
-- > last $ primesToUA (10^8)
-- 99999989
-- (0.32 secs, 467,224,000 bytes)
-- @
primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- Data.Array.Unboxed.assocs $ sieveUA top]

-- |
-- Prints coprime pairs (a,b) with 0<=a<=b<=n, in order. Equivalently, rationals between [0,1]. Uses properties of the mediant, and symmetry about 1/2.
-- 
-- For length, instead, use 'sumTotients'.
-- 
-- @
-- > farey 7
-- [(0,1),(1,7),(1,6),(1,5),(1,4),(2,7),(1,3),(2,5),(3,7),(1,2),(4,7),(3,5),(2,3),(5,7),(3,4),(4,5),(5,6),(6,7),(1,1)]
-- > mapM_ (putStrLn . unwords . map (\(a,b)->show a ++ "/" ++ show b) . farey) [1..7]
-- 0\/1 1\/1
-- 0\/1 1\/2 1\/1
-- 0\/1 1\/3 1\/2 2\/3 1\/1
-- 0\/1 1\/4 1\/3 1\/2 2\/3 3\/4 1\/1
-- 0\/1 1\/5 1\/4 1\/3 2\/5 1\/2 3\/5 2\/3 3\/4 4\/5 1\/1
-- 0\/1 1\/6 1\/5 1\/4 1\/3 2\/5 1\/2 3\/5 2\/3 3\/4 4\/5 5\/6 1\/1
-- 0\/1 1\/7 1\/6 1\/5 1\/4 2\/7 1\/3 2\/5 3\/7 1\/2 4\/7 3\/5 2\/3 5\/7 3\/4 4\/5 5\/6 6\/7 1\/1
-- > map (length . farey) [0,1,10,100,1000]
-- [1,2,33,3045,304193]
-- (0.04 secs, 112,981,608 bytes)
-- > map (sumTotient) [0,1,10,100,1000]  -- wasted worked recalculating the full sum each time.
-- [1,2,33,3045,304193]
-- (0.00 secs, 6,790,920 bytes)
-- > map (sumTotients!!) [0,1,10,100,1000]  -- sumTotients is an infinite list, and there is some memory of values along the way
-- [1,2,33,3045,304193]
-- (0.00 secs, 1,095,320 bytes)
-- @
farey n
  |n == 0 = [(0,1)]
  |n == 1 = [(0,1),(1,1)]
  |otherwise =
     let farey' = go [(0,1),(1,2)] n
     in farey' ++ map (\(a,b)->(b-a,b)) (drop 1 (reverse farey'))
  where
    go [] _ = []
    go [x] _ = [x]
    go (x:y:xs) n
      | q > n = x : go (y:xs) n
      | otherwise = go (x:m:y:xs) n
      where m@(p,q) = mediant x y
            mediant (a,b) (c,d) = (a+c,b+d)

-- | 
-- From Bezout's Lemma, if g = gcd a b, there exist integers s,t, satisfying as + bt = g. 
-- The function will take two Integral values and display the s, t, g that satisfy as+bt==g where g = gcd a b.
-- 
-- @
-- a = q*b+r
-- g = gcd a b = gcd (q*b+r) b = gcd r b = gcd b r
-- bs + rt = g
-- bs + (a-q*b)t = g  
-- at + b(s-qt) = g
-- @
-- 
-- is the basis of the algorithm.
-- Example:
-- @gcdext 3 5 == [2,-1,1] @ as @3*2-1*5 = 1@
gcdext a 0 = [1,0,a]
gcdext a b =
  let (q,r) = a `quotRem` b
      [s,t,g] = gcdext b r
  in [t,s-q*t,abs g]

-- | The command primes generates the infinite list of primes. Algorithm is trial division, so slow for large lists.
-- 
-- @
-- >takeWhile (<100) primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
-- (0.02 secs, 453,616 bytes)
-- >length . takeWhile (<=10^6) $ primes
-- 78498
-- (0.37 secs, 1,445,390,656 bytes)
--  @
primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..] where
  isPrime n =
    let r = floor . sqrt . fromIntegral $ n
    in (n >= 2) && all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes)

-- |
-- primes' is just like primes, but uses Int, and a base 210 wheel instead of sieving on odds. Much faster, great if you know all inputs are bounded by @maxBound :: Int@.
-- 
-- @
-- > takeWhile (<100) primes'
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
-- (0.00 secs, 138,920 bytes)
-- > length . takeWhile (<=10^6) $ primes'
-- 78498
-- (0.04 secs, 27,135,656 bytes)
-- @
primes' :: [Int]
primes' = 2 : filter isPrime wheel where
  isPrime n = let r = floor . sqrt . fromIntegral $ n in all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes')
  wheel  = 3:5:7: drop 1 ( scanl' (+) 1 (cycle wheel'))
  wheel' = [10,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,
           2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2]

-- |
-- Infinite stream of composite numbers. Constructed via list difference of [2..] and primes. See 'setMinus'. This appears slightly faster than filtering by not . isPrime.
-- 
-- @
-- > length . filter (not . isPrime) $ [2..10^6]
-- 921501
-- (0.40 secs, 1,608,183,088 bytes)
-- > length . takeWhile (<=10^6) $ composites
-- 921501
-- (0.36 secs, 625,343,240 bytes)
-- > take 20 composites
-- [4,6,8,9,10,12,14,15,16,18,20,21,22,24,25,26,27,28,30,32]
-- (0.00 secs, 66,784 bytes)
-- @
composites = setMinus [2..] primes

-- | 
-- The sieve for primes is only for odd primes, so a list merge of the odd primes and the even composites is the full list of composites.
compositesTo :: Int -> [Int]
compositesTo top = merge [4,6..top] [i*2+1 | (i,False) <- Data.Array.Unboxed.assocs $ sieveUA top]

-- |
-- Expects two sorted (ascending) lists, but won't throw an error if you input incorrectly. If the left argument becomes empty, the right argument is lazy, so infinite lists should go in the right.
-- 
-- @
-- > setMinus [2..10] [1,3..10]
-- [2,4,6,8,10]
-- (0.00 secs, 61,384 bytes)
-- > setMinus [1..10] [1..5]
-- [6,7,8,9,10]
-- (0.00 secs, 61,072 bytes)
-- > [10,4,3] `setMinus` [1,4,3]
-- [10,4,3]
-- (0.00 secs, 57,200 bytes)
-- > [100..120] `setMinus` primes
-- [100,102,104,105,106,108,110,111,112,114,115,116,117,118,119,120]
-- (0.00 secs, 131,656 bytes)
-- @
setMinus xs [] = xs
setMinus [] _  = []
setMinus (x:xs) (y:ys)
  |x == y  = setMinus xs ys
  |x < y   = x : setMinus xs (y:ys)
  |x > y   = setMinus (x:xs) ys


-- |
-- Trial division check of all primes below the sqrt of the input.
-- 
-- @
-- >isPrime (2^31-1)
-- True
-- @
isPrime :: Integral a => a -> Bool
isPrime n =
  let r = floor . sqrt . fromIntegral $ n
  in (n >= 2) && all (\p -> n `rem` p /= 0) (takeWhile (<= r) primes)

-- | Trial division with an unboxed sieve. Can be faster for large inputs, but requires lots of memory if you're doing this a lot.
isPrime' :: Int -> Bool
isPrime' n = (n >= 2) && ((n == 2) || (not . any (\p -> n `rem` p == 0) . primesToUA . floor . sqrt) (fromIntegral n))



-- |
-- Trial division sieve on integral types. Large inputs may work, but if its divisors are too large, will take forever.
-- 
-- @
-- >primeFactors (2^3*3^5*5^7)
-- [2,2,2,3,3,3,3,3,5,5,5,5,5,5,5]
-- @
primeFactors :: Integral t => t -> [t]
primeFactors n = factors n primes
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

-- |
-- For Ints.
primeFactors' :: Int -> [Int]
primeFactors' n = factors n primes'
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

-- |  Maps a positive integer to its prime factorization as a list of [(prime,powers)]
-- >primePowers (2^3*3^5*5^7)
-- [(2,3),(3,5),(5,7)]
primePowers:: Integral a => a -> [(a,Int)]
primePowers = map (\(p:ps) -> (p, length $ p:ps)) . group . primeFactors

-- | For Ints
primePowers' :: Int -> [(Int,Int)]
primePowers' = map (\(p:ps) -> (p, length $ p:ps)) . group . primeFactors'

-- |
-- Like primePowers, but a list of lists all prime powers which divide the number instead. Used with 'outer' and 'merge' to create a list of all divisors.
-- 
-- @
-- >primePowersListed (2^3*3^5*5^7)
-- [[1,2,4,8],[1,3,9,27,81,243],[1,5,25,125,625,3125,15625,78125]]
-- @
primePowersListed n = map (\(a,b) -> [(a^)] <*> [0..b]) (primePowers n)


-- |
-- Used to multiply numbers together by finding their prime factorizations, then adding powers to multiply.
-- Faster than multiplying first, e.g.:
-- 
-- @
-- > multiplyPrimeFactors [5,7,13,22,10,44]
-- [(2,4),(5,2),(7,1),(11,2),(13,1)]
-- > multiplyPrimeFactors [2]
-- >[(2,1)]
-- > length $ primePowers $ product [1..10000]
-- 1229
-- (0.46 secs, 588,360,224 bytes)
-- > length $ multiplyPrimeFactors [1..10000]
-- 1229
-- (0.05 secs, 127,035,640 bytes)
-- @
-- 
-- Note, the prime factorization of n! has a well known form, which is even faster to compute, so this is better for "random" lists. For each p <= n, take n `div` p repeatedly, and add.
-- 
-- @
-- let fastFactorialFactor n = 
--   let ps = takeWhile (<=n) primes' 
--   in zip ps $ map (\p-> sum . takeWhile (>0) . tail $ iterate (`div` p) n) ps
-- > length $ fastFactorialFactor 10000
-- 1229
-- (0.00 secs, 583,560 bytes)  --compared to 0.05 for multiplyPrimeFactors
-- > take 10 $ multiplyPrimeFactors [1..10000]
-- [(2,9995),(3,4996),(5,2499),(7,1665),(11,998),(13,832),(17,624),(19,554),(23,452),(29,355)]
-- (0.05 secs, 119,797,232 bytes)
-- > take 10 $ fastFactorialFactor 10000
-- [(2,9995),(3,4996),(5,2499),(7,1665),(11,998),(13,832),(17,624),(19,554),(23,452),(29,355)]
-- (0.00 secs, 138,992 bytes)
-- @
multiplyPrimeFactors xs = f . sort $ primePowers =<< xs where
  f [] = []
  f [(p,n)] = [(p,n)]
  f (x@(p,n):(y@(q,m):xs)) = if p == q then f ((p,m+n):xs) else (p,n):f (y:xs)


-- |
-- @
-- > numOfDistinctPrimeFactors $ (2*3*5)^4
-- 3
-- @
numOfDistinctPrimeFactors = length . primePowers

-- |
-- Checks for all even powers in the prime factorization. Unsuitable for numbers with large smallest prime factors. If you need speed, consider
-- 
-- @
-- isSquare n = 
--   let r = floor . sqrt . fromIntegral $ n 
--   in r^2==n
-- @
-- , which will fail for large inputs due to floating point error.
isSquare n =
 (n >= 0) && all (even . snd) (primePowers n)


-- | See 'isSquare'.
isCube n =
 (n >= 0) && all (((==0) . (`mod` 3)) . snd) (primePowers n)

-- | See 'isSquare', but for perfect powers. 
-- 
-- @
-- > filter isPower [1..100]
-- [1,4,8,9,16,25,27,32,36,49,64,81,100]
-- @ 
isPower 1 = True
isPower x =
  let pf@(p:ps) = primePowers x
      powers = map snd pf
      g = foldr1 gcd powers
  in all ((>1) . snd) pf && (null ps || g > 1)

-- |
-- Outer product of two lists with a given function.
-- 
-- @
-- > outer (*) [1,2,4] [1,3,9]
-- [[1,3,9],[2,6,18],[4,12,36]]
-- > foldr1 merge $ outer (*) [1,2,4] [1,3,9]
-- [1,2,3,4,6,9,12,18,36]
-- > divisors 36
-- [1,2,3,4,6,9,12,18,36]
-- > mapM_ print $ outer (\x y -> x*y `mod` 7) [1..6] [1..6]
-- [1,2,3,4,5,6]
-- [2,4,6,1,3,5]
-- [3,6,2,5,1,4]
-- [4,1,5,2,6,3]
-- [5,3,1,6,4,2]
-- [6,5,4,3,2,1]
-- > outer (^) (take 5 primes) [0..4]
-- [[1,2,4,8,16],[1,3,9,27,81],[1,5,25,125,625],[1,7,49,343,2401],[1,11,121,1331,14641]]
-- @
outer f xs ys = [[f x y | y<-ys]|x<-xs]


-- |
-- Takes a number and returns the list of all its integer divisors, in order. Generate by folding an outer product and merging those lists, which are already sorted, so merging is easy. See 'merge'.
-- 
-- Replacing fold with scan you can see the process at work.
-- 
-- @
-- > scanl1 (\x y -> foldr1 merge $ outer (*) x y) (primePowersListed (2^2*3^2*5))
-- [[1,2,4],[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,9,10,12,15,18,20,30,36,45,60,90,180]]
-- > divisors (2^2*3^2*5)
-- [1,2,3,4,5,6,9,10,12,15,18,20,30,36,45,60,90,180]
-- @
divisors 1 = [1]
divisors n =   foldr1 (\x y -> foldr1 merge $ outer (*) x y) (primePowersListed n)

-- |
-- Expects two sorted lists, and merges them. Not lazy in either argument, so while you can merge infinite lists, you have to cut it off.
-- 
-- @
-- > take 10 $ merge primes composites
-- [2,3,4,5,6,7,8,9,10,11]
-- > merge [1..5] [2,4,6]
-- [1,2,2,3,4,4,5,6]
-- @
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x <= y    = x:merge xs (y:ys)
  | otherwise =  y:merge (x:xs) ys

-- | Divisors, not including the input. 
propDivisors n = if n<=1 then [] else init . divisors $ n

-- | Uses the prime factorization directly to calculate the sum of divisors. 
sumDivisors :: Integral a => a -> a
sumDivisors n = if n == 0 then 0 else product $  map (\(a,b)-> (a^(b+1)-1) `div` (a-1)) $ primePowers n

-- | See 'sumDivisors', 'propDivisors' vs 'divisors'
sumPropDivisors :: Integral a => a -> a
sumPropDivisors n = sumDivisors n - n

-- | Amicable pairs are numbers which are each others sum of proper divisors.
-- Amicable Pairs = [(220,284),(1184,1210),(2620,2924),(5020,5564),(6232,6368),..]
-- 
-- @
-- > filter isAmicable [1..10000]
-- [220,284,1184,1210,2620,2924,5020,5564,6232,6368]
-- @
isAmicable  n =
  let m = sumPropDivisors n
  in (m /= n) &&  (sumPropDivisors m == n)

-- |
-- A perfect number is equal to the sum of its proper divisors. An abundant has the sum larger than itself. And a deficient has the sum lower than itself.
-- 
-- @
-- > filter isPerfect [2..8128]
-- [6,28,496,8128]
-- > take 10 . filter isAbundant $ [2..]
-- [12,18,20,24,30,36,40,42,48,54]
-- > take 10 . filter isDeficient $ [2..]
-- [2,3,4,5,7,8,9,10,11,13]
-- @
isPerfect   n = sumPropDivisors n == n
-- | See 'isPerfect' 
isAbundant  n = sumPropDivisors n >  n
-- | See 'isPerfect' 
isDeficient n = sumPropDivisors n <  n


{- |
Initial sketch using lists, not much better than just a filter. Now using Data.IntMap implementation, much faster.

@
abundantsTo n = go [1..n] where
  go [] = []
  go (x:xs) =
    let y = sumPropDivisors x
        mults = [x,2*x..n]
        ys = xs `setMinus` mults
        union xs [] = xs
        union [] ys = ys
        union (x:xs) (y:ys)
          | x <  y    = x:union xs (y:ys)
          | x == y    = x:union xs ys
          | otherwise =  y:union (x:xs) ys
    in case compare x y of
        EQ -> (drop 1 mults) `union` go ys
        LT -> mults `union` go ys
        GT -> go xs
@
-}
abundantsTo :: Int -> [Int]
abundantsTo n = Map.keys $ go Map.empty start where
  start = Map.fromDistinctAscList [(x, ()) | x <- [6..n]]
  go acc xs
    | Map.null xs = acc
    | otherwise =
      let (x, _) = Map.findMin xs
          y = sumPropDivisors x
          mults = Map.fromDistinctAscList [(m, ()) | m <- [x,2*x..n]]
          xs' = if x <= y then Map.difference xs mults else Map.deleteMin xs
          acc' = case compare y x of
            -- organized by frequency: most numbers are deficient, < 25% are abundant, 
            -- we only know something like ~50 even perfect numbers..

            -- deficient does not give us anything, so do not accumulate anything new.
            LT -> acc -- Deficient
            -- every multiple of an abundant number is abundant 
            GT -> Map.union acc mults -- Abundant
            -- throw away the perfect numbers
            -- every multiple of a perfect number is abundant
            EQ -> Map.union acc (Map.deleteMin mults) -- Perfect
      in go acc' xs'

-- |
-- We could generate this list and calculate its length:
-- 
-- @
-- >length $ divisors (2^3*3^5*5^7)
-- 192
-- @
-- 
-- One better way to count divisors is to note for each prime power, we have the choice from 0..n as possible factors.
-- So we can simply add one to each exponent, and take the product of all those exponents.
-- This is implemented in numOfDivisors, numOfPropDivisors.
-- 
-- @
-- >do numOfDivisors (2^3*3^5*5^7); do numOfPropDivisors (2^3*3^5*5^7); do numOfDistinctPrimeFactors (2^3*3^5*5^7);
-- 192
-- 191
-- 3
-- @
-- 
-- Clearly, 4*6*8=192.
numOfDivisors n = product [b+1|(_,b)<-primePowers n]

-- |  See 'numOfDivisors' - 1.
numOfPropDivisors n = numOfDivisors n - 1

-- |
-- Highly composites are numbers (>=1) which have strictly more divisors than any number below them.
-- 
-- @
-- >take 25 $ highlyComposites
-- [1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560,10080,15120,20160,25200,27720]
-- @
highlyComposites =
  let f [] = []; f (x@(a,b):xs) = x:dropWhile ((<=a) . fst) (f xs)
  in  map snd . f $ map (\n->(numOfDivisors n, n)) (1:[2,4..])


-- | 
-- Related to the 'highlyComposites', but they have at least as many divisors as every number below them. 
-- The highly composites are a subset of this by definition.
largelyComposites =
  let f [] = []; f (x@(a,b):xs) = x:dropWhile ((<a) . fst) (f xs)
  in  map snd . f $ map (\n->(numOfDivisors n, n)) (1:2:3:[4,6..])

-- |
-- totient n returns the number of elements 1<= k <= n where gcd k n == 1, but calculated directly from the prime factorization. Equivalently, the number of fractions @0 <= a\/n <= 1@ where a\/n is a fully reduced fraction.
-- 
-- For motivation, simply an example: 30 has 2, 3, and 5 as prime factors. 1/2 of them will be divisible by 2. 1/3 of the remaining by 3. 1/5 of the remaining by 5.
-- 
-- So totient 30 should be 30*(1-1\/2)*(1-1\/3)*(1-1\/5) = 8
-- 
-- And indeed,
-- 
-- @
-- > filter ((==1) . gcd 30) [1..30]
-- [1,7,11,13,17,19,23,29]
-- @
-- But we will not use the list to calculate totient, we use the prime factors instead.
-- 
-- We use the modified form of p^n*(1-1/p) = p^(n-1)*(p-1) and only work on the prime powers themselves.
totient n = foldr ((*) . (\(p,n)-> p^(n-1)*(p-1))) 1 (primePowers n)

-- |
-- The Charmichael function (sometimes "reduced totient") returns the minimum m such that a^m = 1 (mod n) for all a coprime to n. See 'totient' for related.
-- 
-- @
-- >charmichael 12
-- 2
-- @
-- 
-- Note @map (^2) [1,5,7,11] == [1,25,49,121]@
charmichael :: Integral b => b -> b
charmichael n = foldr (lcm . f) 1 (primePowers n) where
    f (p,r)
      | p == 2 && r > 3 = (p^(fromIntegral r-1)*(p-1)) `div` 2
      | otherwise =  p^(fromIntegral r-1)*(p-1)

-- | The summative totient, summing the totient of 1 up to the totient of the input. If you need multiple values, consider the list form 'sumTotients'. See 'farey'.
sumTotient n = foldr ((+) . totient) 1 [1..n]

-- | Infinite list of the sums if you need to print multiple. See 'farey'.
sumTotients = scanl' (+) 1 (map totient [1..])

-- |
-- It can be shown that the multiplicative order of a number a modulo b, i.e.,
-- the least n satisifying a^n `rem` b == 1, must be a divisor of the the totient of b. In fact, the order divides charmichael b which divides totient b.
-- 
-- @
-- >order 2 17
-- 8
-- @
-- As in, 2^8 `rem` 17 == 1
-- 
-- It is expected that gcd a b == 1, but will instead give incorrect results (charmichael b) instead of throwing an exception.
-- 
-- @
-- > order 2 10
-- 4
-- @
-- 
-- Order is also the length of repetition in a decimal fraction. If there are any common factors, strip them out first.
-- 
-- @
-- > order 10 7
-- 6
-- > putStrLn $ prettyPrint (1%7) (1+6*2)
-- 0.142857142857
-- > let fix n = if even n then fix (n `div` 2) else if n `mod` 5 == 0 then fix (n `div` 5) else n
-- > order 10 (fix 250)
-- 1
-- > order 10 (fix 14)
-- 6
-- > order 10 13
-- 6
-- > putStrLn $ prettyPrint (1%13) (1+6*2)
-- 0.076923076923
-- @
order :: Integral p => p -> p -> p
order a b = case test of
  [] -> start
  (t:_) -> t
  where
    start = charmichael b
    test = dropWhile (\n -> modPow a n b 1 /=1) (propDivisors start)

-- |
-- Infinite list of digits generated through the division algorithm. Terminating decimals will end with an infinite tail of 0. 0th-index is the integer part. 
-- 
-- 
-- @
-- > take 13 $ fractionDigits 1 7
-- [0,1,4,2,8,5,7,1,4,2,8,5,7]
-- > take 6 $ fractionDigits 1 8
-- [0,1,2,5,0,0]
-- > take 2 $ fractionDigits 100 4
-- [25,0]
-- @
fractionDigits a b =
  let (p,q) = a `quotRem` b
  in p:fractionDigits (10*q) b

-- |
-- Takes a Ratio r and Integral n and makes a string representation of n digits of the rational r. Note, the integer part will be considered as 1 digit regardless of size.
-- 
-- @
-- > prettyPrint (100%1) (1)
-- "100."
-- > putStrLn $ prettyPrint (100%1) (1)
-- 100.
-- > putStrLn $ prettyPrint (355%113) (1)
-- 3.
-- > putStrLn $ prettyPrint (355%113) (10)
-- 3.141592920
-- > newton a x = 1/2*(x+a/x)
-- > mapM_ (putStrLn . (\r->prettyPrint r 36)) . take 6 $  iterate (newton 17) (4%1)
-- 4.00000000000000000000000000000000000
-- 4.12500000000000000000000000000000000
-- 4.12310606060606060606060606060606060
-- 4.12310562561768349549700032015144555
-- 4.12310562561766054982140985603792501
-- 4.12310562561766054982140985597407702
-- > mapM_ (putStrLn . (\r->prettyPrint (r^2) 36)) . take 6 $  iterate (newton 17) (4%1)
-- 16.00000000000000000000000000000000000
-- 17.01562500000000000000000000000000000
-- 17.00000358700642791551882460973370064
-- 17.00000000000018921488822128189584484
-- 17.00000000000000000000000000052650402
-- 17.00000000000000000000000000000000000
-- @
prettyPrint ratio numDigits=
  if ratio < 0 then '-':prettyPrint (-ratio) numDigits
  else concat . (\(x:xs)->x:".":xs) . map show . take numDigits $ fractionDigits (numerator ratio) (denominator ratio)

-- |
-- Actual implementation now relies on modular powers, the nth decimal digit of a/b is a*(10^n) `mod` (10*b) `div` b `mod` 10. See 'modPow'. Very quick.
-- 
-- @
-- > (fractionDigits 1 3)!!(10^6)
-- 3
-- (0.56 secs, 372,056,032 bytes)
-- > (fractionDigits 1 3)!!(10^7)
-- *** Exception: stack overflow
-- > nthDigit (10^7) 1 3
-- 3
-- (0.00 secs, 59,448 bytes)
-- > nthDigit (10^100) 1 3
-- 3
-- (0.01 secs, 154,016 bytes)
-- @
-- 
-- Original attempt is still somewhat useful, as the information can be used elsewhere. First partition decimals first into it's non-repeating + repeating term, 
-- and then cycle the repeating term if we need to, to keep the lists small.
-- 
-- Fast enough for small numbers, faster than (fractionDigits a b )!!n, but we end up having to calculate ('order 10 n') 
-- which in turn requires us to calculate totient, which requires we know the prime numbers up to n.
-- 
-- gets very slow.
-- 
-- @
-- nthDigit n a b =
--   let nonrepeating = (\x-> if null x then 0 else maximum x) . map  snd . filter ( (`elem` [2,5]) . fst) $ primePowers b
--       repeating = (\xs-> if null xs then 0 else fromIntegral . order 10 $ product xs) . map (uncurry (^)) . filter (\(p,_)->p `notElem` [2,5]) $ primePowers b
--       basis = fractionDigits a b
--       decimals = (\(x:xs)->(x,splitAt nonrepeating xs)) basis
--   in  if n <= nonrepeating then basis!!n
--       else if repeating /= 0 then take repeating ((\(_,(_,c))->c) decimals)!!((n-nonrepeating-1) `mod` repeating) else 0
-- @
nthDigit n a b = a*modPow 10 n (10*b) 1 `div` b  `mod` 10

-- |
-- Standard modular power function, splits into cases. Algorithm based on Edwin Clark's Elementary Number Theory, chapter 26, but use accumulators instead of explicitly storing the binary representation.
-- 
-- The last input is an accumulator that should start at 1 in most cases.
-- 
-- @
-- > 3^12
-- 531441
-- > modPow 3 12 (10^6) 1
-- 531441
-- (0.00 secs, 59,664 bytes)
-- > modPow 3 (3^27) (10^24) 1
-- 75206738945776100739387
-- (0.00 secs, 111,440 bytes)
-- > modPow 7 100 (10^6) 1
-- 60001
-- @
-- 
-- Note 3^3^27 has approximately 3.6 trillion digits.

-- | 
-- Mobius function. Returns 0 for numbers that have a squared prime factor. 
-- For square-free numbers: if we have an even number of prime factors, 1, and odd, -1.
mu x
  | sqf = if even n then 1 else -1
  | otherwise = 0
   where (sqf, ps, n) = (isSquareFree x, primePowers x, length ps)
         isSquareFree x = all ((<2) . snd) ps


modPow b e 1 r = 0
modPow b 0 m r = r
modPow b e m r
  | e `mod` 2 == 1 = modPow b' e' m (r * b `mod` m)
  | otherwise = modPow b' e' m r
  where
    b' = b * b `mod` m
    e' = e `div` 2

-- |
-- See 'fractionDigits', but for abitrary base. Note, bases higher 10 still use Int for each digit.
-- 
-- @
-- > take 10 $ fractionDigitsBase 2 1 3
-- [0,0,1,0,1,0,1,0,1,0]
-- > take 8 $ fractionDigitsBase 10 1 4
-- [0,2,5,0,0,0,0,0]
-- > take 8 $ fractionDigitsBase 16 1 19
-- [0,0,13,7,9,4,3,5]
-- @
fractionDigitsBase n a b =
  let (p,q) = a `quotRem` b
  in undigits (digitsBase n p):fractionDigitsBase n (n*q) b

-- | Turn an integer into a list of digits. Keeps the digits in order.
-- 
-- @
-- > digits 1234
-- [1,2,3,4]
-- @
-- 
-- Equivalent to 'digitsBase 10'
digits = digitsBase 10

-- |
-- Generalized for arbitrary base. Does not support Hex encoding, instead stores as a list of digits.
-- 
-- @
-- > map (digitsBase 2) [0..7]
-- [[0],[1],[1,0],[1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
-- > digitsBase 16 255
-- [15,15]
-- @
digitsBase n a = reverse $ go a where
 go a =
   let (p,q) = a `quotRem` n
   in if p==0 then [q] else q:go p

-- |
-- Intended to have undigits . digits == id, but it functions more akin to string concatenation.
-- 
-- @
-- >undigits [1,2,3,4]
-- 1234
-- > undigits [11,2,3,4]
-- 11234
-- > digits . undigits $ [11,2,3,4]
-- [1,1,2,3,4]
-- > length . digits $ (product [1..10])
-- 7
-- @
undigits = foldl1 (\a b->10*a + b)

-- |
-- Accumulates elements until an element is found or breaks 10^6. Returns the accumulated list not including repeats, in reverse order. Useful when iterating, e.g.,  'sumPropDivisors'.
-- 
-- @
-- > checkLoop [1..4]
-- [4,3,2,1]
-- > checkLoop [1,2,3,4,1]
-- [4,3,2,1]
-- > checkLoop [1,1,2]
-- [1]
-- > mapM_ (print . reverse . checkLoop . (iterate sumPropDivisors)) [6,220,12496,138]  -- 6 is perfect, 220 is an amicable pair, 12496 is a member of an amicable chain.
-- [6]
-- [220,284]
-- [12496,14288,15472,14536,14264]
-- [138,150,222,234,312,528,960,2088,3762,5598,6570,10746,13254,13830,19434,20886,21606,25098,26742,26754,40446,63234,77406,110754,171486,253458,295740,647748]
-- @
checkLoop xs = go [] xs
    where
        go acc [] = acc
        go [] (x:xs) = go [x] xs
        go acc (x:xs)
            | x > 10^6 = acc
            | x `elem` acc = acc
            | otherwise = go (x:acc) xs

-- |
-- A better way to generate amicable pairs than 'isAmicable'.
--
-- Lazy evaluation of (&&) allows us to only calculate the second sum of proper divisors for abundant numbers.
-- Asymptotic density of the even abundant numbers is slightly less than 0.25.
--
-- @
-- > length $ takeWhile ((<=10^6) . snd) amicablePairs
-- 40
-- (1.02 secs, 3,506,830,864 bytes)
-- > length $ filter isAmicable [1..10^6]
-- 82
-- (29.81 secs, 92,461,503,256 bytes)
-- @
-- The number's are only approximately equal because those filters are asking different questions, but the time difference is the key here.
amicablePairs = go [220..] where
  go [] = [];
  go (x:xs) =
    let y = sumPropDivisors x
        z = sumPropDivisors y
    in if x < y && x == z then (x,y):go (xs \\ [y]) else go xs
