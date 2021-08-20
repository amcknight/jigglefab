module Wall
( Wall (..)
, wSide
) where

import Space
import Point
import Vector

data Wall = VLine Float | HLine Float | Circle Position Radius deriving (Eq, Show)

wSide :: Wall -> Position -> Side
wSide (VLine x) p = wSide' x $ fst p
wSide (HLine y) p = wSide' y $ snd p
wSide (Circle c r) p = if distSq p c > r^2 then Out else In
wSide' :: Float -> Float -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out
