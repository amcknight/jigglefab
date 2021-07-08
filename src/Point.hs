module Point
( Point
, pos
, vel
, distSq
, speedSq
) where

import Vector ( lengthSq, Vector )

type Position = Vector
type Velocity = Vector
type Point = (Position, Velocity)

pos :: Point -> Position
pos (p, _) = p

vel :: Point -> Velocity
vel (_, v) = v

distSq :: Point -> Float
distSq = lengthSq . pos

speedSq :: Point -> Float
speedSq = lengthSq . vel
