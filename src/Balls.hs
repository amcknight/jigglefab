module Balls
( Balls
, points
, chems
, buildBalls
, asList
) where

import Points
import Chems
import Ball
import Pair

type Balls = (Ball, Ball)

points :: Balls -> Points
points = bimap point

chems :: Balls -> Chems
chems = bimap chem

buildBalls :: Points -> Chems -> Balls
buildBalls (p1, p2) (c1, c2) = (Ball p1 c1, Ball p2 c2)

asList :: Balls -> [Ball]
asList (l1, l2) = [l1, l2]