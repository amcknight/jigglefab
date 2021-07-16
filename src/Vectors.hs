module Vectors
( Vectors
, (|+)
, (|-)
, (|.)
) where

import Vector

type Vectors = (Vector, Vector)

(|-) :: Vector -> Vector -> Vector
(|-) (x1, y1) (x2, y2) = (x1-x2, y1-y2)

(|+) :: Vector -> Vector -> Vector
(|+) (x1, y1) (x2, y2) = (x1+x2, y1+y2)

(|.) :: Vector -> Vector -> Float
(|.) (x1, y1) (x2, y2) = x1*x2 + y1*y2
