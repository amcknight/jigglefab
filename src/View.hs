module View
  ( View(..)
  )
where

import Model
import Geometry.Vector

data View c = View
  { model :: Model c
  , center :: Position
  , zoom :: Float
  }