module Pane.Pane
( Pane(..)
) where

import Geometry.Vector
import Graphics.Gloss

class Pane p where
  leftClick :: Position -> p -> p
  rightClick :: Position -> p -> p
  mouseMove :: Position -> p -> p
