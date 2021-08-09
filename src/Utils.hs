module Utils
( R
) where

import Control.Monad.State
import System.Random

type R a = State StdGen a