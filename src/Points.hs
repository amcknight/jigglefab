module Points
( Points
, distSq
, furtherThan
, bounce
, side
, hitTime
, sidedHitTime
, movePoints
) where

import Space
import Pair
import Vector
import Vectors
import Point
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

type Points = (Point, Point)

poss :: Points -> (Position, Position)
poss = bimap pos

vels :: Points -> (Velocity, Velocity)
vels = bimap vel

movePoints :: Duration -> Points -> Points
movePoints dt = bimap (movePoint dt)

distSq :: Points -> Float
distSq (p1, p2) = lengthSq $ pos p1 |- pos p2

furtherThan :: Float -> Points -> Bool
furtherThan d ps
  | distSq ps > d^2 = True
  | otherwise = False

side :: Radius -> Points -> Side 
side rad ps = if furtherThan rad ps then Out else In

bounce :: Points -> Points
bounce (p1, p2) = (p3, p4)
  where
    p3 = (pos p1, vel p1 |+ to1 |- to2)
    p4 = (pos p2, vel p2 |+ to2 |- to1)
    to1 = closestPointOnLine (0,0) (pos p2 |- pos p1) (vel p2)
    to2 = closestPointOnLine (0,0) (pos p1 |- pos p2) (vel p1)

hitTime :: Radius -> Points -> Maybe Duration
hitTime rad ps = case root of
  Nothing -> Nothing 
  Just r -> minFuture $ possTimes r
  where
    possTimes :: Float -> (Duration, Duration)
    possTimes r = ((r - s)/totalSpeedSq, (-r - s)/totalSpeedSq)
    
    root :: Maybe Float
    root = safeRoot $ totalSpeedSq * (rad^2 - x^2 - y^2) + s^2

    safeRoot :: Float -> Maybe Float
    safeRoot x
      | x < 0     = Nothing
      | otherwise = Just $ sqrt x

    s = x*v + y*w
    totalSpeedSq = v^2 + w^2
    (p1, p2) = ps
    (x, y) = pos p1 |- pos p2
    (v, w) = vel p1 |- vel p2

earliestHitTime :: Radius -> Points -> Duration
earliestHitTime rad ps = max xt yt
  where
    xt = (dx - rad) / dvx
    yt = (dy - rad) / dvy
    (p1, p2) = poss ps
    (v1, v2) = vels ps
    (dx, dy)   = p2 |- p1
    (dvx, dvy) = v1 |- v2

sidedHitTime :: Duration -> Side -> Radius -> Points -> Duration
sidedHitTime rht desiredSide rad ps = sht
  where
    minDt = 0.00000001
    lowTime = rht - minDt
    highTime = rht + minDt
    lowSide = side rad $ bimap (movePoint lowTime) ps
    roughSide = side rad $ bimap (movePoint rht) ps
    highSide = side rad $ bimap (movePoint highTime) ps
    sht
      | roughSide == desiredSide = rht
      | lowSide == desiredSide = lowTime
      | highSide == desiredSide = highTime
      | otherwise = undefined
