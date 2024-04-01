# Revision history for ProjectEuler

## 1.1.0.1 -- 2024-03-31

* Added solution 59, without original solution. Original used the key.
* Specialized digits functions in Primes.hs, as the duplicate in original 
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

