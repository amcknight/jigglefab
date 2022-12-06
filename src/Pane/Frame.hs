module Pane.Frame
( Frame(..)
, panHop
, zoomHop
, toAbsPos
) where

import Geometry.Vector
import Pane.MousePos
import Util.Side

data Frame = Frame
  { center :: Position
  , zoom :: Double
  }

panHop :: Vector -> Frame -> Frame
panHop dirV f = f {center = center f |+ (hop |* dirV)}
  where hop = 150

zoomHop :: Side -> Frame -> Frame
zoomHop s view = case s of
  Out -> view {zoom = zoom view * zhop}
  In -> view {zoom = zoom view * (1/zhop)}
  where zhop = 1.25

toAbsPos :: Frame -> MousePos -> Position
toAbsPos f mpos = (1 / zoom f) |* (pos mpos |- center f)
