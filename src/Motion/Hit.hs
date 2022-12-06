module Motion.Hit
( Hit (Hit, hitTime, hitSide, ixPair)
) where

import Motion.Time
import Util.Pair
import Data.Ord
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Util.Side

data Hit = Hit
  { hitTime :: Time
  , hitSide :: Side
  , ixPair :: P Int
  } deriving (Eq, Show, Generic)

instance Serialize Hit

instance Ord Hit where
  compare :: Hit -> Hit -> Ordering
  compare = comparing hitTime

instance Mover Hit where
  move :: Duration -> Hit -> Hit
  move dt (Hit d s ip) = Hit (d - dt) s ip
