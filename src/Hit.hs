module Hit
( Hit (Hit, hitTime, hitSide, ixPair)
) where

import Time
import Geometry.Space
import Pair
import Data.Ord

data Hit = Hit
  { hitTime :: Time
  , hitSide :: Side
  , ixPair :: P Int
  } deriving (Eq, Show)

instance Ord Hit where
  compare = comparing hitTime

instance Mover Hit where
  move dt (Hit d s ip) = Hit (d - dt) s ip
