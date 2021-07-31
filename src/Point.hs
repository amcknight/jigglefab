module Point
( Point (Point)
, Position
, Velocity
, pos, vel
, movePoint
) where

import Vector
import Vectors
import Time

type Position = Vector
type Velocity = Vector
data Point = Point
  { pos :: Position
  , vel :: Velocity
  } deriving Show

movePoint :: Duration -> Point -> Point
movePoint dt (Point p v) = Point (p |+ (dt |* v)) v
