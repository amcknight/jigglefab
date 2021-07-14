module Vector
( Vector
, lengthSq
, randomV
, randomVIn
, (|*)
) where

import System.Random

type Vector = (Float, Float)

lengthSq :: Vector -> Float
lengthSq (x, y) = x^2 + y^2

randomV :: StdGen -> Float -> (Vector, StdGen)
randomV seed len = ((x, y), newSeed)
  where
    (theta, newSeed) = randomR (-pi, pi) seed
    x = len * cos theta
    y = len * sin theta


randomVIn :: StdGen -> Float -> (Vector, StdGen)
randomVIn thSeed maxLen = ((x, y), newSeed)
  where
    (theta, lenSeed) = randomR (-pi, pi) thSeed
    (r, newSeed) = randomR (0, 1) lenSeed :: (Float, StdGen)
    len = sqrt r * maxLen
    x = len * cos theta
    y = len * sin theta

(|*) :: Float -> Vector -> Vector
(|*) scale (x, y) = (scale * x, scale * y)
