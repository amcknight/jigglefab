module Geometry.CircleTest
( circlePointsSame
) where

import Geometry.Vector
import Geometry.Circle
import SpecUtils

circlePointsSame :: Position -> Position -> Position -> Bool
circlePointsSame = sym3 circleFrom3
