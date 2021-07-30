{-# LANGUAGE TupleSections #-}
module Pair
( IP
, bimap
, pairs
, pairsTo
, pairsOfTo
, overlaps
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]

pairsTo :: Int -> [(Int, Int)]
pairsTo n = [(x,y) | x <- toN, y <- toN, x < y]
  where toN = [0..(n-1)]

pairsOfTo :: Int -> (Int, Int) -> [(Int, Int)]
pairsOfTo n (i,j) = (i,j) : fmap (,i) low ++ fmap (i,) mid ++ fmap (i,) high ++ fmap (,j) low ++ fmap (,j) mid ++ fmap (j,) high
  where
    low = [0..(i-1)]
    mid = [(i+1)..(j-1)]
    high = [(j+1)..(n-1)]

overlaps :: Eq a => (a, a) -> (a, a) -> Bool 
overlaps (i1, i2) (j1, j2) = i1 == j1 || i1 == j2 || i2 == j1 || i2 == j2
