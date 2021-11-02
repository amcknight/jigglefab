module Geometry.Angle
( Turn
, Radian
, TurnDirection (..)
, AngleType (..)
, Compass(..)
, compass
, tau
, toRadian, toTurn
, degrees, undegrees
, right, left, up, down
, pole
, chord
, simple
, separation
, slope
) where

import Data.Fixed (mod')

type Turn = Float -- From 0 to 1
type Radian = Float -- From -pi to pi
data TurnDirection = Clockwise | CounterClockwise deriving (Show, Eq)
data AngleType = Zero | Acute | Orthogonal | Obtuse | Opposite deriving (Show, Eq)
data Compass = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast deriving (Show, Eq)

compass :: Turn -> Compass
compass t
  | dir == 0 || dir == 1 = East
  | dir == 0.25 = North
  | dir == 0.50 = West
  | dir == 0.75 = South
  | dir > 0.75 = SouthEast
  | dir > 0.50 = SouthWest
  | dir > 0.25 = NorthWest
  | dir > 0.00 = NorthEast
  | otherwise = error "Invalid direction"
  where dir = simple t

tau :: Float
tau = 2*pi

toRadian :: Turn -> Radian
toRadian = (tau*)

toTurn :: Radian -> Turn
toTurn = simple . (/tau)

degrees :: Turn -> Float
degrees = (360*)

undegrees :: Float -> Turn
undegrees = (/360)

pole :: Turn -> Turn
pole = simple . (0.5+)

right :: Turn
right = 0.0
left :: Turn
left = 0.5
up :: Turn
up = 0.25
down :: Turn
down = 0.75

chord :: Radian -> Float 
chord = (2*) . sin . (/2)

simple :: Turn -> Turn
simple t = case compare t' 0 of
  LT -> t'+1
  _ -> t'
  where t' = t `mod'` 1

separation :: Turn -> Turn -> AngleType
separation t1 t2
  | sep == 0 = Zero
  | sep == 0.5 = Opposite
  | sep == 0.25 = Orthogonal
  | sep > 0.25 = Obtuse
  | otherwise = Acute
  where
    rawSep = abs (simple t1 - simple t2)
    sep = if rawSep > 0.5 then 1 - rawSep else rawSep

slope :: Turn -> Float 
slope = tan . toRadian
