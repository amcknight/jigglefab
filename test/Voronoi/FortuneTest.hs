module Voronoi.FortuneTest
( singletonNoEdge
, pairOneEdge
, parallelEdgesFromColinear
, rotatedPointsSame
) where

import Data.List (sort)
import Voronoi.Fortune
import Geometry.Angle
import Test.QuickCheck
import Geometry.Vector
import Voronoi.Edge
import Geometry.Line

singletonNoEdge :: Position -> Bool
singletonNoEdge p = null $ voronoi [p]

pairOneEdge :: Position -> Position -> Bool
pairOneEdge p q = length (voronoi [p,q]) == 1

parallelEdgesFromColinear :: Position -> Float -> Turn -> Property
parallelEdgesFromColinear pos scale turn = (scale /= 0.0) ==> (length (voronoi ps) == 2)
  where
    gap = scale |* unitV turn
    ps = fmap ((pos |+) . (|* gap)) [0, 1, 2]

rotatedPointsSame :: Position -> [Position] -> Turn -> Bool
rotatedPointsSame c ps t = edgePoints (voronoi (fmap (rotateAround c t) ps)) == fmap (rotateAround c t) (edgePoints (voronoi ps))
  where
    edgePoints :: [Edge] -> [Position]
    edgePoints = sort . concatMap (\(Edge (Seg p1 p2) _) -> [p1, p2])
