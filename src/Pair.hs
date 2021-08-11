{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Pair
( P
, bi
, prodTo
, pairsTo
, pairsOfTo1
, pairsOfTo2
, overlaps1
, overlaps2
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
import Time

type P a = (a, a)
instance Mover a => Mover (P a) where
  move dt = bi (move dt)

bi :: (a -> b) -> P a -> P b
bi f (x, y) = (f x, f y)

to :: Int -> [Int]
to n = [0..(n-1)]

prodTo :: Int -> Int -> [P Int]
prodTo n m = [(x,y) | x <- to n, y <- to m]

pairsTo :: Int -> [P Int]
pairsTo n = [(x,y) | x <- to n, y <- to n, x < y]

pairsOfTo1 :: Int -> Int -> [P Int]
pairsOfTo1 n i = fmap (,i) low ++ fmap (i,) high
  where
    low = to i
    high = [(i+1)..(n-1)]

pairsOfTo2 :: Int -> P Int -> [P Int]
pairsOfTo2 n (i,j) = (i,j) : fmap (,i) low ++ fmap (i,) mid ++ fmap (i,) high ++ fmap (,j) low ++ fmap (,j) mid ++ fmap (j,) high
  where
    low = to i
    mid = [(i+1)..(j-1)]
    high = [(j+1)..(n-1)]

overlaps1 :: Eq a => a -> P a -> Bool 
overlaps1 i1 (j1, j2) = i1 == j1 || i1 == j2

overlaps2 :: Eq a => P a -> P a -> Bool
overlaps2 (i1, i2) j = overlaps1 i1 j || overlaps1 i2 j
