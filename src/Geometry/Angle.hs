module Geometry.Angle
( Angle
, TurnDirection (..)
, tau
, turn, unturn
, right, left, up, down
, chord
) where

type Angle = Float
data TurnDirection = Clockwise | CounterClockwise

tau :: Float
tau = 2*pi

turn :: Float -> Angle
turn = (tau*)

unturn :: Angle -> Float 
unturn = (/tau)

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
