module Utils
( R
, sort3
) where

import Control.Monad.State
import System.Random
import Data.List (sort)

type R a = State StdGen a

sort3 :: Ord a => a -> a -> a -> (a, a, a)
sort3 x y z = (a,b,c)
  where [a,b,c] = sort [x,y,z]