module HitTime
( CircleHitTime(..)
, hitTimes
, toList
) where

import Time
import Geometry.Space
import Pair
import Point
import Geometry.Vector

data CircleHitTime = NoHit | InHit Time | OutAndInHit Time Time
toList :: CircleHitTime -> [(Time, Side)]
toList NoHit = []
toList (InHit t) = [(t, In)]
toList (OutAndInHit t1 t2) = [(t1, Out), (t2, In)]

hitTimes :: Radius -> P Point -> CircleHitTime
hitTimes rad ps = maybe NoHit times root
  where
    times :: Float -> CircleHitTime
    times r
      -- Inferring the Side from the time
      | highT < 0 = NoHit
      | lowT < 0 = InHit highT
      | otherwise = OutAndInHit lowT highT
      where
        (lowT, highT) = sortP $ pmap (/speedSq) (r - s, -r - s)

    safeRoot :: Float -> Maybe Float
    safeRoot x = case compare x 0 of
      LT -> Nothing
      EQ -> Just 0
      GT -> Just $ sqrt x

    root = safeRoot $ speedSq * (rad^2 - magnitudeSq (pos diff)) + s^2
    s = pos diff |. vel diff
    speedSq = magnitudeSq $ vel diff
    diff = minus ps
