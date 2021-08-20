module Point
( Point (Point)
, Position
, Velocity
, pos, vel
, side
, minus
, bonkWall, bonkRock, bounce
, birthPoint
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
  | uncurry distSq (pmap pos ps) > d^2 = True
  | otherwise = False

bonkWall :: Ortho -> Point -> Point
bonkWall o p = Point (pos p) $ reflect o $ vel p

bonkRock :: Position -> Radius -> Point -> Point
bonkRock pl r p = p {vel = vel p |+ ((-2) |* velTransferTo p pl) }

bounce :: P Point -> P Point
bounce (p1, p2) = (p3, p4)
  where
    p3 = p1 {vel = vel p1 |+ to1 |- to2} 
    p4 = p2 {vel = vel p2 |+ to2 |- to1} 
    to1 = velTransferTo p2 (pos p1)
    to2 = velTransferTo p1 (pos p2)

velTransferTo :: Point -> Position -> Velocity
velTransferTo (Point p1 v1) p2 = closestPointOnLine (0,0) (p1 |- p2) v1

birthPoint :: Point -> Point -> Point
birthPoint (Point p1 v1) (Point p2 v2) = Point newP newV
  where
    newP = 0.5 |* (p1 |+ p2)
    newV = (-0.5) |* (v1 |+ v2)
