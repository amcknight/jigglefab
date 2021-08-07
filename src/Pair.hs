{-# LANGUAGE TupleSections #-}
module Pair
( IP
, bimap
, prodTo
, pairs
, pairsTo
, pairsOfTo
, pairsOfTo1
, overlaps
, overlaps1
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

prodTo :: Int -> Int -> [(Int, Int)]
prodTo n m = [(x,y) | x <- [0..(n-1)], y <- [0..(m-1)]]

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]

pairsTo :: Int -> [(Int, Int)]
pairsTo n = [(x,y) | x <- toN, y <- toN, x < y]
  where toN = [0..(n-1)]

pairsOfTo1 :: Int -> Int -> [(Int, Int)]
pairsOfTo1 n i = fmap (,i) low ++ fmap (i,) high
  where
    low = [0..(i-1)]
    high = [(i+1)..(n-1)]

pairsOfTo :: Int -> (Int, Int) -> [(Int, Int)]
pairsOfTo n (i,j) = (i,j) : fmap (,i) low ++ fmap (i,) mid ++ fmap (i,) high ++ fmap (,j) low ++ fmap (,j) mid ++ fmap (j,) high
  where
    low = [0..(i-1)]
    mid = [(i+1)..(j-1)]
    high = [(j+1)..(n-1)]

overlaps1 :: Eq a => a -> (a, a) -> Bool 
overlaps1 i1 (j1, j2) = i1 == j1 || i1 == j2

overlaps :: Eq a => (a, a) -> (a, a) -> Bool 
overlaps (i1, i2) (j1, j2) = i1 == j1 || i1 == j2 || i2 == j1 || i2 == j2
