module Pane.Pane
( Pane(..)
) where

import Geometry.Vector
import Pane.Frame

class Pane p where
  leftClick :: Frame -> Position -> p -> p
  rightClick :: Frame -> Position -> p -> p
  mouseMove :: Frame -> Position -> p -> p
