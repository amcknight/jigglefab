{-# LANGUAGE FlexibleInstances #-}

module Geometry.Vector
( Vector
, Position, Velocity
, zeroV
, unit, direction, radians
, upV, downV, rightV, leftV
, upRightV, upLeftV, downRightV, downLeftV
, toUnit
, magnitudeSq
, randomV, randomVs, randomVIn
, (|*), (|+), (|-)
, (|.)
, fromTo, arcFromTo
, distSq, dist
, arcDist
, reflect
, squashTurn
, circleFrom3
, turnDirection
, mid
) where

import Control.Monad.State
import System.Random as R
import Geometry.Angle
import Geometry.Space
import Utils
import Pair

type Vector = P Float
type Position = Vector
type Velocity = Vector

instance Random Vector where
  randomR ((x1,y1),(x2,y2)) g = ((x,y), g3)
    where
      (x, g2) = randomR (x1, x2) g
      (y, g3) = randomR (y1, y2) g2
  random g = (toUnit theta, g2)
    where
      (theta, g2) = randomR (-pi, pi) g

toUnit:: Radian -> Vector
toUnit a = (cos a, sin a)

unit :: Vector -> Vector
unit v = (1 / magnitude v) |* v

zeroV :: Vector
zeroV = (0,0)
upV :: Vector
upV = (0,1)
downV :: Vector
downV = (0,-1)
rightV :: Vector
rightV = (1,0)
leftV :: Vector
leftV = (-1,0)
upRightV :: Vector
upRightV = (1,1)
upLeftV :: Vector
upLeftV = (-1,1)
downRightV :: Vector
downRightV = (1,-1)
downLeftV :: Vector
downLeftV = (-1,-1)

direction :: Vector -> Turn 
direction = toTurn . radians

radians :: Vector -> Radian
radians (x,y) = atan2 y x

magnitudeSq :: Vector -> Float
magnitudeSq v = v |. v

magnitude :: Vector -> Float 
magnitude = sqrt . magnitudeSq

randomV :: Float -> R Vector
randomV len = do
  seed <- get
  let (unit, newSeed) = random seed
  put newSeed
  pure $ len |* unit

randomVs :: Float -> Int -> R [Vector]
randomVs _ 0 = do pure mempty
randomVs len num = do
  vel <- randomV len
  vels <- randomVs len (num-1)
  pure $ vel:vels

randomVIn :: Float -> R Vector
randomVIn maxLen = do
  seed <- get
  let (lenFactor, vSeed) = randomR (0.0, 1.0) seed
  put vSeed
  randomV $ maxLen * sqrt lenFactor

reflect :: Ortho -> Vector -> Vector
reflect Vertical (x,y) = (-x,y)
reflect Horizontal (x,y) = (x,-y)

(|*) :: Float -> Vector -> Vector
(|*) scale = pmap (scale *)

(|-) :: Vector -> Vector -> Vector
(|-) (x1,y1) (x2,y2) = (x1-x2, y1-y2)

(|+) :: Vector -> Vector -> Vector
(|+) (x1,y1) (x2,y2) = (x1+x2, y1+y2)

(|.) :: Vector -> Vector -> Float
(|.) (x1,y1) (x2,y2) = x1*x2 + y1*y2

fromTo :: Vector -> Vector -> Int -> [Vector]
fromTo _ _ 0 = []
fromTo v1 _ 1 = [v1]
fromTo v1 v2 n = v1 : fromTo (v1 |+ hop) v2 (n-1)
  where
    numHops = fromIntegral n - 1
    hop = (1/numHops) |* (v2 |- v1)

arcFromTo :: Radian -> Vector -> Vector -> Int -> [Vector]
arcFromTo _ _ _ 0 = []
arcFromTo _ _ v2 1 = [v2]
arcFromTo _ v1 v2 2 = [v1, v2]
arcFromTo a v1 v2 n = fmap (\i -> rotate (i*gap) c v1) [0..numHops] 
  where
    numHops = fromIntegral $ n - 1
    m = midPoint v1 v2
    v1m = 0.5 |* (v2 |- v1)
    v1mMagSq = magnitudeSq v1m
    cmMagSq = radSqFromArc a v1 v2 - v1mMagSq
    leftCM = (sqrt cmMagSq / sqrt v1mMagSq) |* negOpp v1m
    c = (if a < toRadian 0.5 then (m |+) else (m |-)) leftCM
    gap = a / numHops

rotate :: Radian -> Vector -> Vector -> Vector
rotate a c v = let cv = v |- c
  in c |+ (magnitude cv |* toUnit (a + radians cv))

distSq :: Vector -> Vector -> Float
distSq v1 v2 = magnitudeSq $ v2 |- v1

dist :: Vector -> Vector -> Float
dist v1 v2 = magnitude $ v2 |- v1

arcDist :: Radian -> Vector -> Vector -> Float 
arcDist a v1 v2 = a * sqrt (radSqFromArc a v1 v2)

midPoint :: Vector -> Vector -> Vector
midPoint v1 v2 = 0.5 |* (v2 |+ v1)

negOpp :: Vector -> Vector
negOpp (x,y) = (-y, x)

radSqFromArc :: Radian -> Vector -> Vector -> Float
radSqFromArc a v1 v2 = distSq v1 v2 / chord a ^ 2

squashTurn :: Radius -> Vector -> Vector -> Turn
squashTurn rad v1 v2 = if 2*rad <= d then 0 else toTurn (acos ((d/2)/rad))
  where d = dist v1 v2

circleFrom3 :: Position -> Position -> Position -> Maybe (Position, Radius)
circleFrom3 p@(x1,y1) q@(x2,y2) r@(x3,y3) = if colinear p q r then Nothing else Just (center, dist (x1,y1) center)
  where
    center = (-1/(2*a)) |* (b, c)
    a = x1*(y2-y3) - y1*(x2-x3) + x2*y3 - x3*y2

    b =   (x1^2 + y1^2) * (y3-y2)
        + (x2^2 + y2^2) * (y1-y3)
        + (x3^2 + y3^2) * (y2-y1)
 
    c =   (x1^2 + y1^2) * (x2-x3) 
        + (x2^2 + y2^2) * (x3-x1) 
        + (x3^2 + y3^2) * (x1-x2)

colinear :: Position -> Position -> Position -> Bool 
colinear p q r
  | parallel (q |- p) (r |- p) = True
  | otherwise = False

parallel :: Vector -> Vector -> Bool 
parallel v w = direction v == direction w || direction v == pole (direction w)

turnDirection :: Position -> Position -> Position -> Maybe TurnDirection
turnDirection (x1,y1) (x2,y2) (x3,y3) = case compare det 0 of
  LT -> Just Clockwise
  EQ -> Nothing
  GT -> Just CounterClockwise 
  where det = x1*(y2 - y3) - y1*(x2 - x3) + (x2*y3 - y2*x3)

mid :: Vector -> Vector -> Vector
mid v1 = (0.5 |*) . (v1 |+)
