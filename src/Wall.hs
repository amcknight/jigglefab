module Wall
( Wall (..)
, rock
, wSide
) where

import Geometry.Space
import Point
import Geometry.Vector
import Geometry.Circle

data Wall = VLine Float | HLine Float | Rock Circle deriving (Eq, Show)

rock :: Position -> Radius -> Wall
rock p = Rock . Circle p

wSide :: Wall -> Position -> Side
wSide (VLine x) p = wSide' x $ fst p
wSide (HLine y) p = wSide' y $ snd p
wSide (Rock (Circle c r)) p = if distSq p c > r^2 then Out else In
wSide' :: Float -> Float -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out
