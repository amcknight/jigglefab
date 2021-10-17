{-# LANGUAGE LambdaCase #-}
module Geometry.Voronoi
( Edge(..)
, voronoi
) where

import Geometry.Vector
import Debug.Trace
import Geometry.Line
import Pair
import Geometry.Bound
import Geometry.Beach
import Geometry.Angle
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Either (partitionEithers)
import Data.List (sort)

data Edge = Edge
  { seg :: Seg
  , edgeI :: P Int
  } deriving Show

voronoi :: [Position] -> [Edge]
voronoi ps = edgesFromRays (bufferedBound ps 1) $ voronoi' $ initialBeach ps
voronoi' :: Beach -> [Ray]
voronoi' b@(Beach _ [] _ rs) = S.toList rs
voronoi' b = voronoi' $ updateBeach b

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = pairs ++ fmap (edgeFromRay bnd) strays
  where (pairs, strays) = rayDups rs

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
    edgeRay r1@(Ray p1 _ i1 j1) (Ray p2 _ i2 j2) = if i1 == i2 && j1 == j2
      then Left $ Edge (Seg p1 p2) (i1, j1)
      else Right r1

edgeFromRay :: Bound -> Ray -> Edge
edgeFromRay b (Ray p dir i j) = Edge (Seg p (rayCrossBound b p (simple dir))) (i,j)

rayCrossBound :: Bound -> Position -> Turn -> Position
rayCrossBound ((mxX,mxY),(mnX,mnY)) p@(x,y) dir
  | dir == 0 || dir == 1 = (mxX, y)
  | dir == 0.25 = (x, mxY)
  | dir == 0.50 = (mnX, y)
  | dir == 0.75 = (x, mnY)
  | dir > 0.75 = closer (mxX, yForMaxX) (xForMinY, mnY)
  | dir > 0.50 = closer (mnX, yForMinX) (xForMinY, mnY)
  | dir > 0.25 = closer (mnX, yForMinX) (xForMaxY, mxY)
  | dir > 0.00 = closer (mxX, yForMaxX) (xForMaxY, mxY)
  | otherwise = error "Invalid direction"
  where
    s = slope dir
    b = y - s * x
    yForMinX = s * mnX + b
    xForMinY = (mnY-b) / s
    yForMaxX = s * mxX + b
    xForMaxY = (mxY-b) / s

    closer :: Position -> Position -> Position
    closer a b = if distSq p a < distSq p b then a else b
