module Wall
( Wall (..)
, rock
, wSide
) where

import Geometry.Space
import Point
import Geometry.Vector
import Geometry.Circle

data Wall = VLine Double | HLine Double | Rock Circle deriving (Eq, Show)

rock :: Position -> Radius -> Wall
rock p = Rock . Circle p

wSide :: Wall -> Position -> Side
wSide (VLine x) p = wSide' x $ fst p
wSide (HLine y) p = wSide' y $ snd p
wSide (Rock (Circle c r)) p = if distSq p c > r*r then Out else In
wSide' :: Double -> Double -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out
