module View
  ( View(..)
  )
where

import Model
import Geometry.Vector
import Struct

data View c = View
  { structOrModel :: Either (Struct c) (Model c)
  , center :: Position
  , zoom :: Float
  }

instance AnchorPos (View c) where
  pos = center