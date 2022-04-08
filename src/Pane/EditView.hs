module Pane.EditView
( EditView(..)
, setOverlayOn
) where

import Struct
import Overlay
import Geometry.Vector
import DataType
import Pane.Pane
import Draw
import Graphics.Gloss
import Chem
import qualified Data.Map as V
import Voronoi.Fortune
import Tiling (tileVoronoi)

data EditView c = EditView
  { overlay :: Overlay
  , tip :: Token
  , struct :: Struct c
  }

setOverlayOn :: Position -> EditView c -> EditView c
setOverlayOn p ev = ev {overlay = Overlay p []}

instance Pane (EditView c) where
  leftClick _ = id
  rightClick _ = id
  mouseMove _ = id
