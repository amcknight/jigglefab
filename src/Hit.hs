module Hit
( Hit (Hit)
) where

import Time
import Space
import Pair

data Hit = Hit
  { dur :: Duration
  , sid :: Side 
  , ip :: IP
  } deriving (Eq, Show)

instance Ord Hit where
  compare (Hit dt1 _ _) (Hit dt2 _ _) = compare dt1 dt2
