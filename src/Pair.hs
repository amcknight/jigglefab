{-# LANGUAGE TupleSections #-}

module Pair
( IP
, bimap
, pairsUp
, pairs
) where

type IP = (Int, Int)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

pairsUp :: [a] -> [((a, a), [a])]
pairsUp xs = pairsUp' xs []
  where
    pairsUp' :: [a] -> [a] -> [((a, a), [a])]
    pairsUp' [] _ = []
    pairsUp' [x] _ = []
    pairsUp' [x, y] es = [((x, y), es)]
    pairsUp' (x:xs) es = pairUp x xs es ++ pairsUp' xs (es++[x])

    pairUp :: a -> [a] -> [a] -> [((a, a), [a])]
    pairUp x [] _ = []
    pairUp x [y] es = [((x, y), es)]
    pairUp x (y:ys) es = ((x, y), es++ys) : pairUp x ys (es++[y])

pairs :: Ord a => [a] -> [(a, a)]
pairs ls = [(x,y) | x <- ls, y <- ls, x < y]
