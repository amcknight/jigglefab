module Ball
( Ball (Ball)
, point
, chem
, buildBalls
) where

import Pair
import Point
import Time

data Ball c = Ball
  { point :: Point
  , chem :: c
  } deriving Show

instance Mover (Ball c) where
  move dt (Ball p c) = Ball (move dt p) c

buildBalls :: P Point -> P a -> P (Ball a)
buildBalls (p1, p2) (c1, c2) = (Ball p1 c1, Ball p2 c2)
