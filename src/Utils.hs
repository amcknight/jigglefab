module Utils
( R
, sort3
, allEq, anyEq
) where

import Control.Monad.State
import System.Random
import Data.List (sort)

type R a = State StdGen a

sort3 :: Ord a => a -> a -> a -> (a, a, a)
sort3 x y z = (a,b,c)
  where [a,b,c] = sort [x,y,z]

allEq :: Eq a => [a] -> Bool
allEq []     = True
allEq (x:xs) = all (== x) xs

anyEq :: Ord a => [a] -> Bool
anyEq = anyEq' . sort
anyEq' :: Eq a => [a] -> Bool
anyEq' [] = False
anyEq' [x] = False
anyEq' (x:y:xs) = x == y || anyEq' (x:xs)
