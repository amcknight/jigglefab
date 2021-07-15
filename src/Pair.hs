module Pair
( bimap
, indexedPairs
, IP
) where

type IP = (Int, Int) -- Index Pair

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

indexedPairs :: [a] -> [(IP, (a, a))]
indexedPairs as = filter proper allPairs
  where
    proper :: (IP, (a, a)) -> Bool
    proper ((i1, i2), ls) = i1 < i2
    allPairs = do
      let is = zip [0..] as
      (i1, a1) <- is
      (i2, a2) <- is
      return ((i1, i2), (a1, a2))
