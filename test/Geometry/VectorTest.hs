module Geometry.VectorTest
( midColinear
, lineColinear
, sumIs2Mids
, scaledUnitSame
, sumSymmetric
, rotateIntSame
, allColinearValid
, fromByLength
) where

import Geometry.Vector
import SpecUtils
import Debug.Trace
import Geometry.Angle
import Test.QuickCheck

midColinear :: Position -> Position -> Bool
midColinear p q = colinear p q $ mid p q

lineColinear :: Position -> Vector -> Property
lineColinear p gap = (magnitudeSq gap > 0) ==> colinear p1 p2 p3
  where [p1,p2,p3] = fromBy p gap 3

sumIs2Mids :: Position -> Position -> Bool
sumIs2Mids p q = p |+ q == 2 |* mid p q

scaledUnitSame :: Vector -> Bool
scaledUnitSame v = case unit v of
  Nothing -> v == zeroV
  Just u -> magnitude v |* u == v

sumSymmetric :: Vector -> Vector -> Bool
sumSymmetric = sym2 (|+)

rotateIntSame :: Vector -> Int -> Bool
rotateIntSame v n = rotate v (fromIntegral n) == v

allColinearValid :: Int -> Position -> Turn -> Float -> Bool
allColinearValid n p t d = allColinear ps
  where ps = fmap (rotateAround p t . (\i -> p |+ (fromIntegral i |* (d,0)))) [0..n]

fromByLength :: Position -> Vector -> Int -> Property
fromByLength p gap n = (n >= 0) ==> length (fromBy p gap n) == n
