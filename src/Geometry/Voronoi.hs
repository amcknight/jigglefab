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
import Geometry.CrossPoint
import Data.Maybe (mapMaybe)

data Edge = Edge
  { seg :: Seg
  , edgeI :: P Int
  } deriving Show

voronoi :: [Position] -> [Edge]
voronoi ps = edgesFromRays (bufferedBound ps 1) $ voronoi' $ initialBeach ps
voronoi' :: Beach -> [Ray]
voronoi' b@(Beach _ _ [] _ rs) = rs
voronoi' b = voronoi' $ updateBeach b

edgesFromRays :: Bound -> [Ray] -> [Edge]
edgesFromRays bnd rs = pairs ++ mapMaybe (edgeFromRay bnd) strays
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

edgeFromRay :: Bound -> Ray -> Maybe Edge
edgeFromRay b (Ray p dir i j) = case rayCrossBound b p (simple dir) of 
  NoCross -> Nothing
  OneCross q -> Just $ Edge (Seg p q) (i,j)
  TwoCross q r -> Just $ Edge (Seg q r) (i,j)
  AllCross -> error "A line and bound can't be identical"

rayCrossBound :: Bound -> Position -> Turn -> CrossPoints
rayCrossBound bnd@((mxX,mxY),(mnX,mnY)) p@(x,y) dir = case overlap bnd (boundOne p) of
  Nothing -> if length onBound < 2 then NoCross
    else if length onBound > 2 then error "Three crosspoints on the bound. Possible only in very edgy cases"
    else let [b1, b2] = onBound in TwoCross b1 b2
  Just _ -> OneCross $ case compass dir of
    East -> (mxX, y)
    North -> (x, mxY)
    West -> (mnX, y)
    South -> (x, mnY)
    NorthEast -> closer pForMinX pForMinY
    NorthWest -> closer pForMinX pForMinY
    SouthWest -> closer pForMinX pForMaxY
    SouthEast -> closer pForMinX pForMaxY
  where
    s = slope dir
    b = y - s * x
    mnXY = s * mnX + b
    mxXY = s * mxX + b
    mnYX = (mnY-b) / s
    mxYX = (mxY-b) / s
    pForMinX = (mnX, mnXY)
    pForMinY = (mnYX, mnY)
    pForMaxX = (mxX, mxXY)
    pForMaxY = (mxYX, mxY)
    mnXYIn = mnXY < mxY && mnXY > mnY
    mxXYIn = mxXY < mxY && mxXY > mnY
    mnYXIn = mnYX < mxX && mnYX > mnX
    mxYXIn = mxYX < mxX && mxYX > mnX
    onBound = fmap snd $ filter fst $ zip [mnXYIn, mxXYIn, mnYXIn, mxYXIn] [pForMinX, pForMaxX, pForMinY, pForMaxY]

    closer :: Position -> Position -> Position
    closer a b = if distSq p a < distSq p b then a else b
