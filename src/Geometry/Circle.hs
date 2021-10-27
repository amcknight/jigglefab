module Geometry.Circle
( Circle(..)
) where

import Geometry.Vector
import Geometry.Space

data Circle = Circle
  { circPos :: Position
  , circRad :: Radius
  } deriving (Show, Eq)
