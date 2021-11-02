module Voronoi.Fortune
( voronoi
) where

import Geometry.Vector
import Debug.Trace
import Geometry.Line
import Geometry.Bound
import Voronoi.Beach
import Geometry.Angle
import Geometry.CrossPoint
import Voronoi.Edge

voronoi :: [Position] -> [Edge]
voronoi [p,q] = case unsidedRayCrossBound b m d of
  TwoCross p1 p2 -> [Edge (Seg p1 p2) (0,1)]
  _ -> error "Segment must exist"
  where
    b = bufferedBound [p,q] 1
    m = mid p q
    d = simple $ 0.25 + direction (p |- m)
voronoi ps = edgesFromRays (bufferedBound ps 1) $ voronoi' $ initialBeach ps

voronoi' :: Beach -> [Ray]
voronoi' b@(Beach _ _ [] _ rs) = rs
voronoi' b = voronoi' $ updateBeach b
