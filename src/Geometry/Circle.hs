module Geometry.Circle
( Circle(..)
, circleFrom3
) where

import Geometry.Vector
import Geometry.Space
import Data.List (sort)
import Utils

data Circle = Circle
  { circPos :: Position
  , circRad :: Radius
  } deriving (Show, Eq)

instance AnchorPos Circle where
  pos = circPos

circleFrom3 :: Position -> Position -> Position -> Maybe Circle
circleFrom3 p1 p2 p3 = if colinear 5 [p,q,r] then Nothing else Just $ Circle center $ dist p center
  where
    (p@(px,py),q@(qx,qy),r@(rx,ry)) = sort3 p1 p2 p3
    center = (-1/(2*a)) |* (b, c)
    a = px*(qy-ry) - py*(qx-rx) + qx*ry - rx*qy

    b =   (px^2 + py^2) * (ry-qy)
        + (qx^2 + qy^2) * (py-ry)
        + (rx^2 + ry^2) * (qy-py)

    c =   (px^2 + py^2) * (qx-rx)
        + (qx^2 + qy^2) * (rx-px)
        + (rx^2 + ry^2) * (px-qx)
  