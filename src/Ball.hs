module Ball
( Ball (Ball)
, point
, chem
, buildBall
, buildBalls
) where

import Pair
import Point
import Time
import Orb
import Geometry.Vector
import Utils

-- TODO: Should this be an Orb with a velocity?
data Ball c = Ball
  { point :: Point
  , chem :: c
  } deriving Show

instance Mover (Ball c) where
  move dt (Ball p c) = Ball (move dt p) c

buildBall :: Speed -> Orb c -> R (Ball c)
buildBall sp (Orb p c) = do
  v <- randomV sp
  pure $ Ball (Point p v) c

buildBalls :: P Point -> P a -> P (Ball a)
buildBalls (p1, p2) (c1, c2) = (Ball p1 c1, Ball p2 c2)
