module Geometry.Space
( Radius
, Ortho (..)
, Side (..)
, Sided
, flipSide
, flipSided
, swapPair
, inPair, inSide
) where

import Pair
import Data.Tuple

type Radius = Double
data Ortho = Vertical | Horizontal deriving (Eq, Show)
data Side = Out | In deriving (Show, Eq, Ord)
type Sided a = (P a, Side)

flipSide :: Side -> Side
flipSide In = Out
flipSide Out = In

flipSided :: Sided a -> Sided a
flipSided (p, s) = (p, flipSide s)

swapPair :: Sided a -> Sided a
swapPair (p, s) = (swap p, s)

inPair :: Eq a => Sided a -> a -> Bool
inPair ((i, j), _) x = x == i || x == j

inSide :: Sided a -> Bool
inSide (_, s) = s == In
