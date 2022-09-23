module Pane.Pane
( Pane(..)
) where

import Pane.Frame
import Pane.MousePos

class Pane p where
  leftClick :: Frame -> MousePos -> p -> p
  rightClick :: Frame -> MousePos -> p -> p
  mouseMove :: Frame -> MousePos -> p -> p
