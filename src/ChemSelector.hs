module ChemSelector
( ChemSelector (..)
, drawOverlay
) where

import Geometry.Space
import Geometry.Vector
import Graphics.Gloss.Interface.IO.Interact
import Chem

newtype ChemSelector = ChemSelector Type

drawOverlay :: ChemSelector -> Picture
drawOverlay (ChemSelector (Type name _)) = Text name
-- hitsOverlay :: Position -> Side
-- handleEvent :: Event -> Picture -> Picture
