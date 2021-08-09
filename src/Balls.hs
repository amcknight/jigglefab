module Balls
( Balls
, points
, chems
, buildBalls
) where

import Points
import Ball
import Pair
import Chemy

type Balls a = (Ball a, Ball a)

points :: Balls a -> Points
points = bimap point

chems :: Balls a -> Chemies a
chems = bimap chem

buildBalls :: Points -> Chemies a -> Balls a
buildBalls (p1, p2) (c1, c2) = (Ball p1 c1, Ball p2 c2)
