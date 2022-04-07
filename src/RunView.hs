module RunView
( RunView(..)
) where

import Model
import System.Random

data RunView c = RunView
  { seed :: StdGen
  , model :: Model c
  }
