module Geometry.Angle
( Turn
, Radian
, TurnDirection (..)
, tau
, toRadian, toTurn
, degrees, undegrees
, right, left, up, down
, pole
, chord
, simple
) where

import Data.Fixed (mod')

type Turn = Float -- From 0 to 1
type Radian = Float -- From -pi to pi
data TurnDirection = Clockwise | CounterClockwise

tau :: Float
tau = 2*pi

toRadian :: Turn -> Radian
toRadian = (tau*)

toTurn :: Radian -> Turn
toTurn = (/tau)

degrees :: Turn -> Float 
degrees = (360*)

undegrees :: Float -> Turn
undegrees = (/360)

pole :: Turn -> Turn
pole = (`mod'` 1) . (0.5+)

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
