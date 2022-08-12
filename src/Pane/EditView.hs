module Pane.EditView
( EditView(..)
) where

import Struct
import Geometry.Vector
import DataType
import Pane.Pane
import Debug.Trace

data EditView c = EditView
  { tip :: Token
  , hover :: Maybe Int
  , struct :: Struct c
  }

instance Pane (EditView c) where
  leftClick mpos ev = ev
  rightClick mpos ev = ev
-- TODO: Remove magic numbers
  mouseMove (mx, my) ev = if mx > -1880 && mx < -1420 && my < 1020
    then ev {hover = Just i}
    else ev {hover = Nothing}
    where i = floor $ (1020 - my) / 40
