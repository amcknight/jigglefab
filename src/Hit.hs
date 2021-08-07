module Hit
( Hit (Hit, hitTime, hitSide, ixPair)
, moveHit
) where

import Time
import Space
import Pair

data Hit = Hit
  { hitTime :: Time
  , hitSide :: Side
  , ixPair :: IP
  } deriving (Eq, Show)

instance Ord Hit where
  compare h1 h2 = compare (hitTime h1) (hitTime h2)

moveHit :: Duration -> Hit -> Maybe Hit
moveHit dt (Hit d s ip)
  | newDur < 0 = Nothing
  | otherwise = Just $ Hit newDur s ip
  where newDur = d - dt
