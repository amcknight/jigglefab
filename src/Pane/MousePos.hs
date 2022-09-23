module Pane.MousePos
( MousePos(..)
) where

import Geometry.Vector

newtype MousePos = MousePos Position

instance HasPos MousePos where
  pos :: MousePos -> Position
  pos (MousePos p) = p
