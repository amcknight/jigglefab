module Geometry.Angle
( Angle
, TurnDirection (..)
, tau
, turn, unturn
, degrees, undegrees
, right, left, up, down
, pole
, chord
) where

import Data.Fixed (mod')
type Angle = Float -- From 0 to 1
data TurnDirection = Clockwise | CounterClockwise

tau :: Float
tau = 2*pi

turn :: Float -> Angle
turn = (tau*)

unturn :: Angle -> Float 
unturn = (/tau)

degrees :: Angle -> Float 
degrees = (360*)

undegrees :: Float -> Angle
undegrees = (/360)

pole :: Angle -> Angle
pole = (`mod'` 1) . (0.5+)

right :: Angle
right = 0.0
left :: Angle
left = turn 0.5
up :: Angle
up = turn 0.25
down :: Angle
down = turn 0.75

chord :: Angle -> Float 
chord = (2*) . sin . (/2)
