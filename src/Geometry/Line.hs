module Geometry.Line
( Line(..)
, Seg(..)
, segCrosses
, crossPointsAtUnit
) where
import Geometry.Vector
import Pair
import Geometry.Angle
import Geometry.Bound

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

crossPointsAtUnit :: Line -> Maybe (Position, Position)
crossPointsAtUnit (Line (px, py) (qx, qy)) = case compare discriminant 0 of
  LT -> Nothing
  _ -> Just ((1/disSq) |* (scale |+ preXY), (1/disSq) |* (scale |- preXY))
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
