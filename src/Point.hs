module Point
( Point (Point)
, Position
, Velocity
, pos, vel
, side
, minus
, bonk, bounce
) where

import Vector
import Time
import Space
import Pair
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

type Position = Vector
type Velocity = Vector
data Point = Point
  { pos :: Position
  , vel :: Velocity
  } deriving Show

instance Mover Point where
  move dt (Point p v) = Point (p |+ (dt |* v)) v

minus :: P Point -> Point
minus (Point p1 v1, Point p2 v2) = Point (p1 |- p2) (v1 |- v2)

side :: Radius -> P Point -> Side
side rad ps = if furtherThan rad ps then Out else In

furtherThan :: Float -> P Point -> Bool
furtherThan d ps
  | uncurry distSq (bi pos ps) > d^2 = True
  | otherwise = False

bonk :: Ortho -> Point -> Point
bonk o p = Point (pos p) $ reflect o $ vel p

bounce :: P Point -> P Point
bounce (Point p1 v1, Point p2 v2) = (p3, p4)
  where
    p3 = Point p1 (v1 |+ to1 |- to2)
    p4 = Point p2 (v2 |+ to2 |- to1)
    to1 = closestPointOnLine (0,0) (p2 |- p1) v2
    to2 = closestPointOnLine (0,0) (p1 |- p2) v1
