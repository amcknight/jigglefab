module Wall
( Wall (..)
, wSide
, wallV, wallH
) where

import Space
import Point
import Vector

data Wall = Wall Ortho Float | Rock Position Radius deriving (Eq, Show)

wSide :: Wall -> Position -> Side
wSide (Wall Vertical   pl) p = wSide' pl $ fst p
wSide (Wall Horizontal pl) p = wSide' pl $ snd p
wSide (Rock pl r) p = if distSq p pl > r^2 then Out else In
wSide' :: Float -> Float -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out

wallV :: Float -> Wall
wallV = Wall Vertical

wallH :: Float -> Wall
wallH = Wall Horizontal
