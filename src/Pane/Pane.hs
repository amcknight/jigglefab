module Pane.Pane
( Pane(..)
) where

import Pane.Frame
import Types

class Pane p where
  leftClick :: Frame -> Pos 'Screen -> p -> p
  rightClick :: Frame -> Pos 'Screen -> p -> p
  mouseMove :: Frame -> Pos 'Screen -> p -> p
