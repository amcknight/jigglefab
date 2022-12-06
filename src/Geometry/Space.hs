module Geometry.Space
( Ortho (..)
, Sided
, flipSided
, swapPair
, inPair, inSide
) where

import Util.Pair
import Data.Tuple
import Util.Side

data Ortho = Vertical | Horizontal deriving (Eq, Show)
type Sided a = (P a, Side)

flipSided :: Sided a -> Sided a
flipSided (p, s) = (p, flipSide s)

swapPair :: Sided a -> Sided a
swapPair (p, s) = (swap p, s)

inPair :: Eq a => Sided a -> a -> Bool
inPair ((i, j), _) x = x == i || x == j

inSide :: Sided a -> Bool
inSide (_, s) = s == In
