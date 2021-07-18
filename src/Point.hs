module Point
( Point
, Position
, Velocity
, pos
, posAt
, vel
, movePoint
) where

import Vector
import Vectors
import Time

type Position = Vector
type Velocity = Vector
type Point = (Position, Velocity)

pos :: Point -> Position
pos (p, _) = p

posAt :: Time -> Point -> Position
posAt t = pos . movePoint t

vel :: Point -> Velocity
vel (_, v) = v

movePoint :: Duration -> Point -> Point
movePoint dt (p, v) = (p |+ (dt |* v) , v)
