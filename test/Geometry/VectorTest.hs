module Geometry.VectorTest
( midColinear
, sumIs2Mids
, scaledUnitSame
, sumSymmetric
, rotateIntSame
) where

import Geometry.Vector
import SpecUtils

midColinear :: Position -> Position -> Bool
midColinear p q = colinear p q $ mid p q

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
