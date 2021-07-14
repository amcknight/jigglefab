module Point
( Point
, Position
, Velocity
, pos
, vel
, speedSq
, move
) where

import Vector
import Vectors
import Space (Duration)

type Position = Vector
type Velocity = Vector
type Point = (Position, Velocity)

pos :: Point -> Position
pos (p, _) = p

vel :: Point -> Velocity
vel (_, v) = v

speedSq :: Point -> Float
speedSq = lengthSq . vel

move :: Duration -> Point -> Point
move dt (p, v) = (p |+ (dt |* v) , v)
