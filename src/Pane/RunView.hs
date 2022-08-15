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
  leftClick f mpos rv = rv
  rightClick f mpos rv = rv
  mouseMove f mpos rv = rv
