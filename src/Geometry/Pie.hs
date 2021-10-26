module Geometry.Pie
( Pie(..)
) where

import Geometry.Vector
import Geometry.Line
import Geometry.Sweep

data Pie = Pie
  { piePos :: Position
  , pieSweep :: Sweep
  } deriving Show
