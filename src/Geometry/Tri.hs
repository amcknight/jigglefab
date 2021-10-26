module Geometry.Tri
( Tri(..)
, makeCCW
) where

import Geometry.Vector
import Geometry.Line
import Geometry.Angle (TurnDirection(Clockwise, CounterClockwise))

data Tri = Tri
  { triPos :: Position
  , triSeg :: Seg
  } deriving Show

makeCCW :: Tri -> Tri
makeCCW (Tri o (Seg p q)) = Tri o $ case turnDirection o p q of
  Nothing -> error "Tri Can't be colinear at this point"
  Just Clockwise -> Seg q p
  Just CounterClockwise -> Seg p q
