module Voronoi.Pie
( Pie(..)
) where

import Geometry.Vector
import Voronoi.Sweep

data Pie = Pie
  { piePos :: Position
  , pieSweep :: Sweep
  } deriving Show

instance HasPos Pie where
  pos = piePos
