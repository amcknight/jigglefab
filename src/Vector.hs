module Vector
( Vector (V)
, coords
, zeroV
, lengthSq
, randomV
, randomVs
, randomVIn
, (|*)
, reflect
) where

import System.Random as R
import Space

data Vector = V Float Float deriving Show

instance Random Vector where
  randomR (V x1 y1, V x2 y2) g = (V x y, g3)
    where
      (x, g2) = randomR (x1, x2) g
      (y, g3) = randomR (y1, y2) g2
  random g = (fromAngle theta, g2)
    where
      (theta, g2) = randomR (-pi, pi) g

type Angle = Float

fromAngle :: Angle -> Vector
fromAngle a = V (cos a) (sin a)

coords :: Vector -> (Float, Float)
coords (V x y) = (x, y)

zeroV :: Vector
zeroV = V 0 0

lengthSq :: Vector -> Float
lengthSq (V x y) = x^2 + y^2

randomV :: Float -> StdGen -> (StdGen, Vector)
randomV len seed = (newSeed, len |* unit)
  where (unit, newSeed) = random seed

randomVs :: Float -> Int -> StdGen -> (StdGen, [Vector])
randomVs _ 0 seed = (seed, [])
randomVs len num seed = (newSeed, vel:vels)
  where
    (tailSeed, vel) = randomV len seed
    (newSeed, vels) = randomVs len (num-1) tailSeed

randomVIn :: Float -> StdGen -> (StdGen, Vector)
randomVIn maxLen seed = randomV (maxLen * sqrt lenFactor) vSeed
  where (lenFactor, vSeed) = randomR (0.0, 1.0) seed

(|*) :: Float -> Vector -> Vector
(|*) scale (V x y) = V (scale * x) (scale * y)

reflect :: Ortho -> Vector -> Vector
reflect Vertical (V x y) = V (-x) y
reflect Horizontal (V x y) = V x (-y)
