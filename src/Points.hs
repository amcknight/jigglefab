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
) where

import Space
import Time
import Pair
import Vector
import Vectors
import Point
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

type Points = (Point, Point)

minus :: Points -> Point
minus (Point p1 v1, Point p2 v2) = Point (p1 |- p2) (v1 |- v2)

movePoints :: Duration -> Points -> Points
movePoints dt = bimap (movePoint dt)

-- distSq :: Points -> Float
-- distSq (p1, p2) = lengthSq $ pos p1 |- pos p2

furtherThan :: Float -> Points -> Bool
furtherThan d ps
  | distSq (bimap pos ps) > d^2 = True
  | otherwise = False

side :: Radius -> Points -> Side 
side rad ps = if furtherThan rad ps then Out else In

bounce :: Points -> Points
bounce (Point p1 v1, Point p2 v2) = (p3, p4)
  where
    p3 = Point p1 (v1 |+ to1 |- to2)
    p4 = Point p2 (v2 |+ to2 |- to1)
    -- TODO: Write my own closestPointOnLine
    to1 = uncurry V $ closestPointOnLine (0,0) (coords (p2 |- p1)) (coords v2)
    to2 = uncurry V $ closestPointOnLine (0,0) (coords (p1 |- p2)) (coords v1)

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
