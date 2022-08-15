module Pane.Frame
( Frame(..)
, panHop
, zoomHop
, toAbsPos
) where

import Geometry.Vector
import Geometry.Space

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

toAbsPos :: Frame -> Position -> Position
toAbsPos f p = (1 / zoom f) |* (p |- center f)
