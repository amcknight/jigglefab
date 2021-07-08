module Link
( Link (Link)
, Chem (Chem)
, point
, chem
, moveLink
, hitTime
, bounce
) where

import Point
import Vector
import Space
import Graphics.Gloss.Geometry.Line (closestPointOnLine)

-- Chem Wanted Has
data Chem = Chem Int Int deriving Show
data Link = Link Point Chem deriving Show

point :: Link -> Point
point (Link p _) = p

chem :: Link -> Chem
chem (Link _ c) = c

moveLink :: Duration -> Link -> Link
moveLink dt (Link (p, v) state) = Link (p |+ (dt |* v) , v) state

hitTime :: Radius -> Link -> Link -> Maybe Duration
hitTime rad l1 l2 = case root of
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
    (x, y) = pos (point l1) |- pos (point l2)
    (v, w) = vel (point l1) |- vel (point l2)

earliestHitTime :: Float -> Link -> Link -> Float
earliestHitTime rad l1 l2 = max xt yt
  where
    xt = (dx - rad) / dvx
    yt = (dy - rad) / dvy
    (dx, dy)   = pos (point l2) |- pos (point l1)
    (dvx, dvy) = vel (point l1) |- vel (point l2)

bounce :: Link -> Link -> (Link, Link)
bounce (Link p1 c1) (Link p2 c2) = (l3, l4)
  where
    l3 = Link (pos p1, vel p1 |+ to1 |- to2) c1
    l4 = Link (pos p2, vel p2 |+ to2 |- to1) c2
    to1 = closestPointOnLine (0,0) (pos p2 |- pos p1) (vel p2)
    to2 = closestPointOnLine (0,0) (pos p1 |- pos p2) (vel p1)
