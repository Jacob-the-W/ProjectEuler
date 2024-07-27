# Revision history for ProjectEuler

## 1.1.0.6 2024-7-27-24
* Added problems 70, 72, 73, 74, 76.

## 1.1.0.5 - 2024-05-05
* Added problem 100. Not a good algorithm, but my initial attempt took 26 hours, down to about 40s
* Added problem 75 later.

## 1.1.0.4 - 2024-05-04

* In 'Primes' library, sumTotients has changed to [Int], specialization wasn't working, and is much faster.
* Fixed some haddock documentation that was formatting functions as hypertext, errors involving < and >.
* Problem 60 has been changed to a recursive algorithm instead of a manual psuedo-recursion.
* Added problems 62, 63, 65, 66, 67, 69, 70, 71

## 1.1.0.3 - 2024-4-13

* Problem 60 added

## 1.1.0.2 -- 2024-04-05

* Changed some type signatures. In particular, numOfDivisors no longer returns Int values in case of larger numbers.

## 1.1.0.1 -- 2024-03-31

* Added solution 59, without original solution. Original used the key.
* Specialized digits functions in Primes.hs, as the duplicate in original solution 59 was faster as it was only for Int. 
* Changed 54 to make Read instances for Card, Rank, Suit instead of custom read functions.
* Added type signatures

## 1.1.0.0 -- 2024-03-31

* Updated Primes.hs for haddock, may consider uploading haddock documentation, but 'cabal haddock-project' will generate it.
* Updated .cabal file to include a library for Primes haddock documentation.
* Updated gcdext to return Integral a => (a, a, a) instead of a list, to appease the compiler warnings.
* The lists involved always have 3 arguments.
* Updated amicablePairs to be [(Int,Int)] again, 'SPECIALIZE' wasn't working, and [(Int, Int)] is faster.
* Updated app/prjEuler45.hs, added comments

Times (see code view):
<!--
Before:
> length $ takeWhile ((<=10^6) . snd) amicablePairs
40
(3.57 secs, 11,644,020,696 bytes)
> length $ takeWhile ((<=10^6) . snd) (amicablePairs :: [(Int, Int)])
40
(3.02 secs, 11,693,074,336 bytes)

After:
> length $ takeWhile ((<=10^6) . snd) amicablePairs
40
(0.33 secs, 1,367,095,560 bytes)

-->

## 1.0.0.0 -- 2024-03-30

* Deleted many duplicate functions with ' and instead made use of type classes and GHC 'SPECIALIZE' options.
* Converted jumble of .hs files to cabal project directory

## 0.1.0.0 -- Previously

* Not formatted for cabal. Many repetitions were able to be removed in reworking of Primes.hs.

