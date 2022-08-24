module Pane.RunView
( RunView(..)
) where
import System.Random
import Model
import Pane.Pane

data RunView c = RunView
  { seed :: StdGen
  , model :: Model c
  }

instance Pane (RunView c) where
  leftClick _ _ rv = rv
  rightClick _ _ rv = rv
  mouseMove _ _ rv = rv
