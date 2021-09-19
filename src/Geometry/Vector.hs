{-# LANGUAGE FlexibleInstances #-}

module Geometry.Vector
( Vector
, zeroV
, unit
, upV, downV, rightV, leftV
, upRightV, upLeftV, downRightV, downLeftV
, magnitudeSq
, randomV, randomVs, randomVIn
, (|*), (|+), (|-)
, (|.)
, fromTo, arcFromTo
, distSq, dist
, arcDist
, reflect
) where

import System.Random as R
import Geometry.Angle
import Geometry.Space
import Control.Monad.State
import Utils
import Pair
import Debug.Trace
import Data.Tuple

type Vector = P Float

instance Random Vector where
  randomR ((x1,y1),(x2,y2)) g = ((x,y), g3)
    where
      (x, g2) = randomR (x1, x2) g
      (y, g3) = randomR (y1, y2) g2
  random g = (toUnit theta, g2)
    where
      (theta, g2) = randomR (-pi, pi) g

toUnit:: Angle -> Vector
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

angle :: Vector -> Angle
angle (x,y) = atan2 y x

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

arcFromTo :: Angle -> Vector -> Vector -> Int -> [Vector]
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
    c = (if a < turn 0.5 then (m |+) else (m |-)) leftCM
    gap = a / numHops

rotate :: Angle -> Vector -> Vector -> Vector
rotate a c v = let cv = v |- c
  in c |+ (magnitude cv |* toUnit (a + angle cv))

distSq :: Vector -> Vector -> Float
distSq v1 v2 = magnitudeSq $ v2 |- v1

dist :: Vector -> Vector -> Float
dist v1 v2 = magnitude $ v2 |- v1

arcDist :: Angle -> Vector -> Vector -> Float 
arcDist a v1 v2 = a * sqrt (radSqFromArc a v1 v2)

midPoint :: Vector -> Vector -> Vector
midPoint v1 v2 = 0.5 |* (v2 |+ v1)

negOpp :: Vector -> Vector
negOpp (x,y) = (-y, x)

radSqFromArc :: Angle -> Vector -> Vector -> Float
radSqFromArc a v1 v2 = distSq v1 v2 / chord a ^ 2
