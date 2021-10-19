module Utils
( R
) where

import Control.Monad.State
import System.Random
import Data.List (sort)

type R a = State StdGen a
