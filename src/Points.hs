module Points
( Points
, Interval
, distSq
, minus
, furtherThan
, bounce
, side
, hitTimes
, movePoints
, interval
, tightenInterval
) where

import Space
import Time ( Interval, Duration, Time, minFuture )
import Pair
import Vector
import Vectors
import Point
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

type Points = (Point, Point)

minus :: Points -> Point
minus ((p1, v1),(p2, v2)) = (p1 |- p2, v1 |- v2)

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

hitTimes :: Radius -> Points -> [(Side, Duration)]
hitTimes rad ps = case root of
  Nothing -> [] 
  Just r -> times $ possTimes r
  where
    times :: (Time, Time) -> [(Side, Duration)]
    times (t1, t2)
      | highT < 0 = []
      | lowT < 0 = [(In, highT)]
      | otherwise = [(Out, lowT), (In, highT)]
      where
        (lowT, highT) = if t1 < t2 then (t1, t2) else (t2, t1)

    root :: Maybe Float
    root = safeRoot $ speedSq * (rad^2 - lengthSq (pos diff)) + s^2
    
    possTimes :: Float -> (Time, Time)
    possTimes r = ((r - s)/speedSq, (-r - s)/speedSq)

    safeRoot :: Float -> Maybe Float
    safeRoot x
      | x < 0     = Nothing
      | otherwise = Just $ sqrt x

    s = pos diff |. vel diff
    speedSq = lengthSq $ vel diff
    diff = minus ps

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

interval :: Time -> Radius -> Points -> Maybe Interval
interval t rad p = if wh then Just (initialInterval rad s refP) else Nothing
  where
    wh = case s of
      In -> True 
      Out -> willHit t (rad^2) refP
    refP = minus p
    s = side rad p

-- Assumes Out side
willHit :: Time -> Float -> Point -> Bool 
willHit t radSq p = ct > t && lengthSq (pos (movePoint ct p)) < radSq
  where ct = closestTime p

closestTime :: Point -> Time
closestTime ((x,y),(v,w)) = negate $ (x*v + y*w) / (v^2 + w^2) 

-- Assumes we know they will collide
initialInterval :: Radius -> Side -> Point -> Interval
initialInterval rad In p = (0, uncurry min (bimap (2*rad/) (vel p)))
initialInterval rad Out p = (0, closestTime p)

tightenInterval :: Float -> Side -> Point -> Interval -> Interval
tightenInterval radSq s p (lowT, highT) = if s == newS then (midT, highT) else (lowT, midT)
  where
    midT = (lowT + highT) / 2
    midDistSq = lengthSq (pos (movePoint midT p))
    newS = if radSq > midDistSq then In else Out
