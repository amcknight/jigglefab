module Pane.EditView
( EditView(..)
) where

import Struct
import Geometry.Vector
import DataType
import Pane.Pane

data EditView c = EditView
  { tip :: Token
  , struct :: Struct c
  }

instance Pane (EditView c) where
  leftClick mpos ev = ev
  rightClick mpos ev = ev
  mouseMove mpos ev = ev
