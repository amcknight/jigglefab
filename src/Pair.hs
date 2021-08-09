{-# LANGUAGE TupleSections #-}
module Pair
( IP
, bimap
, prodTo
, pairsTo
, pairsOfTo1
, pairsOfTo2
, overlaps1
, overlaps2
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

prodTo :: Int -> Int -> [IP]
prodTo n m = [(x,y) | x <- [0..(n-1)], y <- [0..(m-1)]]

pairsTo :: Int -> [IP]
pairsTo n = [(x,y) | x <- toN, y <- toN, x < y]
  where toN = [0..(n-1)]

pairsOfTo1 :: Int -> Int -> [IP]
pairsOfTo1 n i = fmap (,i) low ++ fmap (i,) high
  where
    low = [0..(i-1)]
    high = [(i+1)..(n-1)]

pairsOfTo2 :: Int -> IP -> [IP]
pairsOfTo2 n (i,j) = (i,j) : fmap (,i) low ++ fmap (i,) mid ++ fmap (i,) high ++ fmap (,j) low ++ fmap (,j) mid ++ fmap (j,) high
  where
    low = [0..(i-1)]
    mid = [(i+1)..(j-1)]
    high = [(j+1)..(n-1)]

overlaps1 :: Eq a => a -> (a, a) -> Bool 
overlaps1 i1 (j1, j2) = i1 == j1 || i1 == j2

overlaps2 :: Eq a => (a, a) -> (a, a) -> Bool
overlaps2 (i1, i2) j = overlaps1 i1 j || overlaps1 i2 j
