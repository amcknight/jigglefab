module Vectors
( Vectors
, (|+)
, (|-)
, (|.)
, fromTo
, distSq
, dist
) where

import Vector

type Vectors = (Vector, Vector)

(|-) :: Vector -> Vector -> Vector
(|-) (V x1 y1) (V x2 y2) = V (x1-x2) (y1-y2)

(|+) :: Vector -> Vector -> Vector
(|+) (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)

(|.) :: Vector -> Vector -> Float
(|.) (V x1 y1) (V x2 y2) = x1*x2 + y1*y2

fromTo :: Vector -> Vector -> Int -> [Vector]
fromTo _ _ 0 = []
fromTo v1 _ 1 = [v1]
fromTo (V x1 y1) (V x2 y2) n = zipWith V (fromTo' x1 x2 n) (fromTo' y1 y2 n) 
  where
    fromTo' :: Float -> Float -> Int -> [Float]
    fromTo' a b n = [a, a + (b - a) / fromIntegral n .. b]

distSq :: Vectors -> Float 
distSq (v1, v2) = lengthSq $ v2 |- v1

dist :: Vectors -> Float
dist = sqrt . distSq
