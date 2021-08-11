{-# LANGUAGE FlexibleInstances #-}

module Vector
( Vector
, zeroV
, lengthSq
, randomV
, randomVs
, randomVIn
, (|*)
, (|+)
, (|-)
, (|.)
, fromTo
, distSq
, dist
, reflect
) where

import System.Random as R
import Space
import Control.Monad.State
import Utils
import Pair

type Vector = P Float

instance Random Vector where
  randomR ((x1,y1),(x2,y2)) g = ((x,y), g3)
    where
      (x, g2) = randomR (x1, x2) g
      (y, g3) = randomR (y1, y2) g2
  random g = (fromAngle theta, g2)
    where
      (theta, g2) = randomR (-pi, pi) g

type Angle = Float

fromAngle :: Angle -> Vector
fromAngle a = (cos a, sin a)

zeroV :: Vector
zeroV = (0,0)

lengthSq :: Vector -> Float
lengthSq (x,y) = x^2 + y^2

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
(|*) scale (x,y) = (scale * x, scale * y)

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
    hop = bi (/ numHops) (v2 |- v1)

distSq :: Vector -> Vector -> Float 
distSq v1 v2 = lengthSq $ v2 |- v1

dist :: Vector -> Vector -> Float
dist v1 v2 = sqrt $ distSq v1 v2
