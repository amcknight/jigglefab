module Pane.MousePos
( MousePos(..)
, buildMousePos
) where

import Geometry.Vector
import Pair

newtype MousePos = MousePos Position

instance HasPos MousePos where
  pos :: MousePos -> Position
  pos (MousePos p) = p

buildMousePos :: P Float -> MousePos
buildMousePos mpos = MousePos (pmap realToFrac mpos)
