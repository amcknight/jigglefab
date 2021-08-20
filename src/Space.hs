module Space
( Radius
, Ortho (Vertical, Horizontal)
, Side (In, Out)
, Sided
, flipSide
, flipSided
, swapPair
, isIn
) where

import Pair
import Data.Tuple
type Radius = Float

data Ortho = Vertical | Horizontal deriving (Eq, Show)
data Side = Out | In deriving (Show, Eq, Ord)

flipSide :: Side -> Side
flipSide In = Out
flipSide Out = In

type Sided a = (P a, Side)

flipSided :: Sided a -> Sided a
flipSided (p, s) = (p, flipSide s)

swapPair :: Sided a -> Sided a
swapPair (p, s) = (swap p, s)

isIn :: Sided a -> Bool
isIn (_, s) = s == In
