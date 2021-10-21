module Geometry.Line
( Line(..)
, Seg(..)
, segCrosses
, crossPointsAtUnit
, rayCrossBound
, segInBound
) where

import Geometry.Vector
import Geometry.Angle
import Geometry.Bound ( Bound, overlap, boundOne, isIn )
import Geometry.CrossPoint
import Debug.Trace

data Line = Line Position Position deriving Show
data Seg = Seg Position Position deriving Show

segBound :: Seg -> Bound
segBound (Seg p q) = (p,q)

segCrosses :: Seg -> Seg -> Bool
segCrosses (Seg a b) (Seg c d) = case overlap (a,b) (c,d) of
  Nothing -> False 
  Just bound -> case lineCross (Line a b) (Line c d) of
    Nothing -> False
    Just cross -> case overlap bound (boundOne cross) of
      Nothing -> False
      Just _ -> True
      
lineCross :: Line -> Line -> Maybe Position
lineCross (Line (x1,y1) (x2,y2)) (Line (x3,y3) (x4,y4))
  | m21 == m43 = Nothing 
  | otherwise = Just (x,y)
  where
    x21 = x2-x1
    x43 = x4-x3
    y21 = y2-y1
    y43 = y4-y3
    m21 = y21/x21
    m43 = y43/x43
    b = y1-m21*x1
    x = ((y3-y1)*x43*x21 + x1*y21*x43 - x3*y43*x21) / (y21*x43 - y43*x21)
    y = m21*x+b

crossPointsAtUnit :: Line -> CrossPoints
crossPointsAtUnit (Line (px, py) (qx, qy)) = case compare discriminant 0 of
  LT -> NoCross
  EQ -> OneCross $ (1/disSq) |* scale
  GT -> TwoCross ((1/disSq) |* (scale |+ preXY)) ((1/disSq) |* (scale |- preXY))
  where
    dx = qx - px
    dy = qy - py
    disSq = dx^2 + dy^2
    detPQ = px*qy - py*qx
    discriminant = disSq - detPQ^2
    ysign = case compare dy 0 of
      LT -> -1
      _ -> 1

    root = sqrt discriminant
    preXY = root |* (ysign * dx, abs dy)
    scale = detPQ |* (dy, -dx)

rayCrossBound :: Bound -> Position -> Turn -> CrossPoints
rayCrossBound bnd@((mxX,mxY),(mnX,mnY)) p@(x,y) dir
  | isIn bnd p = OneCross $ case compass dir of
    East -> (mxX, y)
    North -> (x, mxY)
    West -> (mnX, y)
    South -> (x, mnY)
    NorthEast -> closer pForMaxX pForMaxY
    NorthWest -> closer pForMinX pForMaxY
    SouthWest -> closer pForMinX pForMinY
    SouthEast -> closer pForMaxX pForMinY
  | length onBound < 2 = NoCross
  | length onBound > 2 = error "TODO: Three crosspoints on the bound. Possible only in very edgy cases"
  | otherwise = case separation dir (direction (b1 |- p)) of
    Zero -> TwoCross b1 b2
    Acute -> TwoCross b1 b2
    _ -> NoCross 
  where
    s = slope dir -- breaks on vertical or horizontal
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
    [b1, b2] = onBound -- Only used after checking length is two

    closer :: Position -> Position -> Position
    closer a b = if distSq p a < distSq p b then a else b

segInBound :: Bound -> Seg -> Maybe Seg
segInBound bnd@((mxX,mxY),(mnX,mnY)) (Seg p@(px,py) q@(qx,qy))
  | isIn bnd p && isIn bnd q = Just $ Seg p q
  | length onBound < 2 = Nothing -- Completely outside
  | length onBound > 2 = error "Three crosspoints on the bound. Possible only in very edgy cases"
  | isIn bnd p = case separation (direction (q |- p)) (direction (b1 |- p)) of
    Zero -> Just $ Seg p b1
    Acute -> Just $ Seg p b1
    _ -> Just $ Seg p b2
  | isIn bnd q = case separation (direction (p |- q)) (direction (b1 |- q)) of
    Zero -> Just $ Seg q b1
    Acute -> Just $ Seg q b1
    _ -> Just $ Seg q b2
  | otherwise = error "Two cross points but Seg is outside. Sounds impossible"
  where
    s = (qy-py)/(qx-px) -- breaks on vertical or horizontal
    b = py - s * px
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
    [b1, b2] = onBound -- Only used after checking the length is 2 so should be ok
