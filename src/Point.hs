module Point
( Point (Point)
, Position
, Velocity
, pos, vel
, movePoint
, bonk
) where

import Vector
import Vectors
import Time
import Space

type Position = Vector
type Velocity = Vector
data Point = Point
  { pos :: Position
  , vel :: Velocity
  } deriving Show

movePoint :: Duration -> Point -> Point
movePoint dt (Point p v) = Point (p |+ (dt |* v)) v

bonk :: Ortho -> Point -> Point
bonk o p = Point (pos p) $ reflect o $ vel p
