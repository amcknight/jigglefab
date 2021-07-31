module Vector
( Vector (V)
, coords
, zeroV
, lengthSq
, randomV
, randomVIn
, (|*)
) where

import System.Random as R

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

randomV :: StdGen -> Float -> (Vector, StdGen)
randomV seed len = (len |* unit, newSeed)
  where (unit, newSeed) = random seed

randomVIn :: StdGen -> Float -> (Vector, StdGen)
randomVIn seed maxLen = randomV vSeed $ maxLen * sqrt lenFactor
  where (lenFactor, vSeed) = randomR (0.0, 1.0) seed

(|*) :: Float -> Vector -> Vector
(|*) scale (V x y) = V (scale * x) (scale * y)
