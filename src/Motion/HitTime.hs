module Motion.HitTime
( CircleHitTime(..)
, hitTimes
, asList
) where

import Motion.Time
import Util.Pair
import Motion.Point
import Geometry.Vector
import Util.Side
import Geometry.Circle

data CircleHitTime = NoHit | InHit Time | OutAndInHit Time Time
asList :: CircleHitTime -> [(Time, Side)]
asList NoHit = []
asList (InHit t) = [(t, In)]
asList (OutAndInHit t1 t2) = [(t1, Out), (t2, In)]

hitTimes :: Radius -> P Point -> CircleHitTime
hitTimes rad ps = maybe NoHit times root
  where
    times :: Double -> CircleHitTime
    times r
      -- Inferring the Side from the time
      | highT < 0 = NoHit
      | lowT < 0 = InHit highT
      | otherwise = OutAndInHit lowT highT
      where
        (lowT, highT) = sortP $ pmap (/speedSq) (r - s, -r - s)

    safeRoot :: Double -> Maybe Double
    safeRoot x = case compare x 0 of
      LT -> Nothing
      EQ -> Just 0
      GT -> Just $ sqrt x

    root = safeRoot $ speedSq * (rad*rad - magnitudeSq (pos diff)) + s*s
    s = pos diff |. vel diff
    speedSq = magnitudeSq $ vel diff
    diff = minus ps
