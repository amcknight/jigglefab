module Wall
( Wall (Wall, ortho, place)
, wSide
, Bonk (Bonk, bonkTime, bonkSide, ixPair)
) where

import Time
import Space
import Link
import Pair
import Point
import Vector

data Wall = Wall
  { ortho :: Ortho
  , place :: Float
  } deriving (Eq, Show)

wSide :: Wall -> Position -> Side
wSide (Wall Vertical pl) (V x _) = wSide' pl x
wSide (Wall Horizontal pl) (V _ y) = wSide' pl y
wSide' :: Float -> Float -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out

data Bonk = Bonk
  { bonkTime :: Time
  , bonkSide :: Side
  , ixPair :: IP
  } deriving (Eq, Show)

instance Ord Bonk where
  compare b1 b2 = compare (bonkTime b1) (bonkTime b2)
