module Hit
( Hit (Hit, dur, sid, ixPair)
, moveHit
) where

import Time
import Space
import Pair

data Hit = Hit
  { dur :: Duration
  , sid :: Side 
  , ixPair :: IP
  } deriving (Eq, Show)

instance Ord Hit where
  compare (Hit dt1 _ _) (Hit dt2 _ _) = compare dt1 dt2

moveHit :: Duration -> Hit -> Maybe Hit
moveHit dt (Hit d s ip)
  | newDur < 0 = Nothing
  | otherwise = Just $ Hit newDur s ip
  where newDur = d - dt
