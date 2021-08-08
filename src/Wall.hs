module Wall
( Wall (Wall, ortho, place)
, wSide
) where

import Time
import Space
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
