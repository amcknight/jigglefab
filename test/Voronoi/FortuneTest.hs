module Voronoi.FortuneTest
( singletonNoEdge
, pairOneEdge
) where

import Geometry.Vector
import Voronoi.Fortune

singletonNoEdge :: Position -> Bool
singletonNoEdge p = null $ voronoi [p]

pairOneEdge :: Position -> Position -> Bool
pairOneEdge p q = length (voronoi [p,q]) == 1
