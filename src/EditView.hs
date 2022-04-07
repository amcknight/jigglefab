module EditView
( EditView(..)
, setOverlayOn
) where

import Struct
import Overlay
import Geometry.Vector
import DataType

data EditView c = EditView
  { overlay :: Overlay
  , tip :: Token
  , struct :: Struct c
  }

setOverlayOn :: Position -> EditView c -> EditView c
setOverlayOn p ev = ev {overlay = Overlay p []}
