module Voronoi.Pie
( Pie(..)
) where

import Geometry.Vector
import Geometry.Line
import Voronoi.Sweep

data Pie = Pie
  { piePos :: Position
  , pieSweep :: Sweep
  } deriving Show
