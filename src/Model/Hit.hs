module Model.Hit
( Hit (Hit, hitTime, hitSide, ixPair)
) where

import Time
import Geometry.Space
import Pair
import Data.Ord
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Hit = Hit
  { hitTime :: Time
  , hitSide :: Side
  , ixPair :: P Int
  } deriving (Eq, Show, Generic)

instance Serialize Hit

instance Ord Hit where
  compare = comparing hitTime

instance Mover Hit where
  move dt (Hit d s ip) = Hit (d - dt) s ip
