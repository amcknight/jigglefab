{-# LANGUAGE TupleSections #-}

module Pair
( IP
, bimap
, pairs
, pairsOf
, overlaps
) where

type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]

pairsOf :: Ord a => [a] -> (a, a) -> [(a, a)]
pairsOf ls (i, j) = [(x, y) | x <- ls, y <- ls, x < y, overlaps (i, j) (x, y)]

overlaps :: Eq a => (a, a) -> (a, a) -> Bool 
overlaps (i1, i2) (j1, j2) = i1 == j1 || i1 == j2 || i2 == j1 || i2 == j2
