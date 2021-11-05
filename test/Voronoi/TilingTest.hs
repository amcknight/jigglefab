module Voronoi.TilingTest
( offEdgeNothing
) where

import Test.QuickCheck
import Geometry.Vector
import Geometry.Angle
import Voronoi.Tiling
import Data.Maybe (isNothing)
import Geometry.CrossPoint

offEdgeNothing :: Position -> Vector -> Position -> Property
offEdgeNothing p gap o = (magnitudeSq gap > 0 && not (colinear o p1 p2)) ==> isNothing (buildTri p1 p2 (TwoCross c1 c2) o)
  where [p1, p2, c1, c2] = fromBy p gap 4
