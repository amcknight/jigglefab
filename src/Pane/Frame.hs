module Pane.Frame
( Frame(..)
, panHop
, zoomHop
, toAbsPos
) where

import Geometry.Vector
import Geometry.Space
import Types
import qualified Geometry.Vector as V

data Frame = Frame
  { center :: Position
  , zoom :: Double
  }

panHop :: Vector -> Frame -> Frame
panHop dirV f = f {center = center f |+ (hop V.|* dirV)}
  where hop = 150

zoomHop :: Side -> Frame -> Frame
zoomHop s view = case s of
  Out -> view {zoom = zoom view * zhop}
  In -> view {zoom = zoom view * (1/zhop)}
  where zhop = 1.25

toAbsPos :: Frame -> Pos 'Screen -> Pos 'World
toAbsPos f (UnsafePos p) = UnsafePos $ (1 / zoom f) V.|* (p |- center f)
