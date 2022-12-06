module Model.Point
( Point (Point)
, Position
, Velocity
, pos, vel
, side
, minus
, bonkVLine, bonkHLine, bonkCircle, bounce
, birthPoint
) where

import Geometry.Vector
import Model.Time
import Geometry.Space
import Util.Pair
import Graphics.Gloss.Geometry.Line (closestPointOnLine)
import Geometry.Circle
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Util.Side

data Point = Point
  { pointPos :: Position
  , vel :: Velocity
  } deriving (Show, Generic)

instance Serialize Point

instance HasPos Point where
  pos = pointPos

instance Mover Point where
  move dt (Point p v) = Point (p |+ (dt |* v)) v

minus :: P Point -> Point
minus (Point p1 v1, Point p2 v2) = Point (p1 |- p2) (v1 |- v2)

side :: P Point -> Side
side ps = if furtherThan 1 ps then Out else In

furtherThan :: Double -> P Point -> Bool
furtherThan d ps
  | uncurry distSq (pmap pos ps) > d*d = True
  | otherwise = False

bonkVLine :: Point -> Point
bonkVLine p = Point (pos p) $ reflect Vertical $ vel p

bonkHLine :: Point -> Point
bonkHLine p = Point (pos p) $ reflect Horizontal $ vel p

bonkCircle :: Circle -> Point -> Point
bonkCircle c p = p {vel = vel p |+ ((-2) |* velTransferTo p (pos c)) }

bounce :: P Point -> P Point
bounce (p1, p2) = (p3, p4)
  where
    p3 = p1 {vel = vel p1 |+ to1 |- to2} 
    p4 = p2 {vel = vel p2 |+ to2 |- to1} 
    to1 = velTransferTo p2 (pos p1)
    to2 = velTransferTo p1 (pos p2)

-- TODO Use a ClosestPointOnLine that works with Doubles
velTransferTo :: Point -> Position -> Velocity
velTransferTo (Point p1 v1) p2 = pmap realToFrac $ closestPointOnLine (0,0) (pmap realToFrac (p1 |- p2)) (pmap realToFrac v1)

birthPoint :: Point -> Point -> Point
birthPoint (Point p1 v1) (Point p2 v2) = Point newP newV
  where
    newP = 0.5 |* (p1 |+ p2)
    newV = (-0.5) |* (v1 |+ v2)
