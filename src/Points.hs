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

minus :: Point -> Point -> Point
minus (p1, v1) (p2, v2) = (p1 |- p2, v1 |- v2)

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
    root :: Maybe Float
    root = safeRoot $ speedSq * (rad^2 - lengthSq (pos diff)) + s^2
    
    possTimes :: Float -> (Duration, Duration)
    possTimes r = ((r - s)/speedSq, (-r - s)/speedSq)

    safeRoot :: Float -> Maybe Float
    safeRoot x
      | x < 0     = Nothing
      | otherwise = Just $ sqrt x

    s = pos diff |. vel diff
    speedSq = lengthSq $ vel diff
    diff = uncurry minus ps

sidedHitTime :: Duration -> Side -> Radius -> Points -> Duration
sidedHitTime rht desiredSide rad ps = sht
  where
    minDt = 0.00000001 --ugly
    lowTime = rht - minDt
    highTime = rht + minDt
    lowSide = side rad $ bimap (movePoint lowTime) ps
    roughSide = side rad $ bimap (movePoint rht) ps
    highSide = side rad $ bimap (movePoint highTime) ps
    sht
      | roughSide == desiredSide = rht
      | lowSide == desiredSide = lowTime
      | highSide == desiredSide = highTime
      | otherwise = undefined --ugly

-- Unused
earliestHitTime :: Radius -> Points -> Duration
earliestHitTime rad ps = max xt yt
  where
    xt = (dx - rad) / dvx
    yt = (dy - rad) / dvy
    (p1, p2) = bimap pos ps
    (v1, v2) = bimap vel ps
    (dx, dy)   = p2 |- p1
    (dvx, dvy) = v1 |- v2 -- Is this right?...