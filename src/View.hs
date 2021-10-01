module View
  ( View(..)
  )
where

import Model
import Geometry.Vector
import Struct

data View c = View
  { model :: Either (Struct c) (Model c)
  , center :: Position
  , zoom :: Float
  }