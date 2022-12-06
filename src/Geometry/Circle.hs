module Geometry.Circle
( Circle(..)
, circleFrom3
) where

import Geometry.Vector
import Geometry.Space
import Util.Utils
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Circle = Circle
  { circPos :: Position
  , circRad :: Radius
  } deriving (Show, Eq, Generic)

instance Serialize Circle

instance HasPos Circle where
  pos = circPos

circleFrom3 :: Position -> Position -> Position -> Maybe Circle
circleFrom3 p1 p2 p3 = if colinear 5 [p,q,r] then Nothing else Just $ Circle center $ dist p center
  where
    (p@(px,py),q@(qx,qy),r@(rx,ry)) = sort3 p1 p2 p3
    center = (-1/(2*a)) |* (b, c)
    a = px*(qy-ry) - py*(qx-rx) + qx*ry - rx*qy

    b =   (px*px + py*py) * (ry-qy)
        + (qx*qx + qy*qy) * (py-ry)
        + (rx*rx + ry*ry) * (qy-py)

    c =   (px*px + py*py) * (qx-rx)
        + (qx*qx + qy*qy) * (rx-px)
        + (rx*rx + ry*ry) * (px-qx)
  