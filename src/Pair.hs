{-# LANGUAGE TupleSections #-}

module Pair
( IP
, bimap
, pairs
, pairsOf
, overlaps
) where

import Data.Maybe (mapMaybe)
import Data.Containers.ListUtils (nubOrd)

type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]

pairsOf :: Ord a => [a] -> (a, a) -> [(a, a)]
pairsOf ls (i, j) = nubOrd $ zoo ls i ++ zoo ls j

zoo :: Ord a => [a] -> a -> [(a, a)]
zoo ls i = mapMaybe (foo i) ls

foo :: Ord a => a -> a -> Maybe (a, a)
foo n i = case compare n i of
  LT -> Just (n, i)
  EQ -> Nothing
  GT -> Just (i, n)


overlaps :: Eq a => (a, a) -> (a, a) -> Bool 
overlaps (i1, i2) (j1, j2) = i1 == j1 || i1 == j2 || i2 == j1 || i2 == j2