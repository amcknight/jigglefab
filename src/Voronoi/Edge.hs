module Voronoi.Edge
( Edge(..)
, Ray(..)
, edgesFromRays
, awayRay
) where

import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers)
import Data.List (sort)
import Pair
import Geometry.CrossPoint
import Geometry.Line
import Geometry.Bound
import Geometry.Vector
import Geometry.Angle
import Utils

data Edge = Edge
  { seg :: Seg
  , edgeI :: P Int
  } deriving (Show, Eq)

data Ray = Ray
  { rayPos :: Position
  , raydir :: Turn
  , rayI :: P Int
   } deriving (Show, Eq)

instance AnchorPos Ray where
  pos = rayPos

instance Ord Ray where
  compare (Ray _ _ (i1, j1)) (Ray _ _ (i2, j2)) = case compare i1 i2 of
    EQ -> compare j1 j2
    o -> o

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = mapMaybe (edgeInBound bnd) pairs ++ mapMaybe (edgeFromRay bnd) strays
  where (pairs, strays) = rayDups rs

edgeInBound :: Bound -> Edge -> Maybe Edge
edgeInBound b (Edge s is) = fmap (`Edge` is) (segInBound b s)

edgeFromRay :: Bound -> Ray -> Maybe Edge
edgeFromRay b (Ray p dir is) = case rayCrossBound b p (simple dir) of
  NoCross -> Nothing
  OneCross q -> Just $ Edge (Seg p q) is
  TwoCross q r -> Just $ Edge (Seg q r) is
  InfinteCross -> error "A line and bound can't be identical"

rayDups :: [Ray] -> ([Edge], [Ray])
rayDups rs = partitionEithers $ addRayDups (sort rs) []

addRayDups :: [Ray] -> [Either Edge Ray] -> [Either Edge Ray]
addRayDups [] ers = ers
addRayDups [r] ers = Right r : ers
addRayDups (r1:r2:rs) ers = case edgeRay r1 r2 of
  Left e -> Left e : addRayDups rs ers
  Right r -> Right r : addRayDups (r2:rs) ers
  where
    edgeRay :: Ray -> Ray -> Either Edge Ray
    edgeRay r1@(Ray p1 _ (i1,j1)) (Ray p2 _ (i2,j2)) = if i1 == i2 && j1 == j2
      then Left $ Edge (Seg p1 p2) (i1, j1)
      else Right r1

awayRay :: Position -> Position -> Position -> Position -> Turn
awayRay o away p q
  | anyEq [o, away, p, q] = error "awayRay: all points should be unique"
  | otherwise = awayTurn (direction (away |- o)) (direction (p |- o)) (direction (q |- o))
  where dir = direction $ mid p q |- o