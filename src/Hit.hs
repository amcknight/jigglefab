module Hit
( Hit (Hit, hitTime, hitSide, ixPair)
) where

import Time
import Space
import Pair

data Hit = Hit
  { hitTime :: Time
  , hitSide :: Side
  , ixPair :: P Int
  } deriving (Eq, Show)

instance Ord Hit where
  compare h1 h2 = compare (hitTime h1) (hitTime h2)

instance Mover Hit where
  move dt (Hit d s ip) = Hit (d - dt) s ip
