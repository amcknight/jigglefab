module Points
( Points
, distSq
, furtherThan
, bounce
, hitTime
) where

import Space
import Vector
import Vectors
import Point
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

type Points = (Point, Point)

poss :: Points -> (Position, Position)
poss (p1, p2) = (pos p1, pos p2)

distSq :: Points -> Float
distSq (p1, p2) = lengthSq $ pos p1 |- pos p2

furtherThan :: Float -> Points -> Bool
furtherThan d ps
  | distSq ps > d^2 = True
  | otherwise = False

bounce :: Points -> Points
bounce (p1, p2) = (p3, p4)
  where
    p3 = (pos p1, vel p1 |+ to1 |- to2)
    p4 = (pos p2, vel p2 |+ to2 |- to1)
    to1 = closestPointOnLine (0,0) (pos p2 |- pos p1) (vel p2)
    to2 = closestPointOnLine (0,0) (pos p1 |- pos p2) (vel p1)

hitTime :: Radius -> Points -> Maybe (Side, Duration)
hitTime rad ps = case root of
  Nothing -> Nothing 
  Just r -> case minFuture $ possTimes r of
    Nothing -> Nothing 
    Just dt -> Just (if furtherThan rad ps then Out else In, dt)
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