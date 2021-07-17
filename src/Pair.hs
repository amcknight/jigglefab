{-# LANGUAGE TupleSections #-}

module Pair
( IP
, bimap
, pairs
) where

type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]
