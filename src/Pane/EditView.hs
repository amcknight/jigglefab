module Pane.EditView
( EditView(..)
, setOverlayOn
) where

import Struct
import Overlay
import Geometry.Vector
import DataType
import Pane.Pane

data EditView c = EditView
  { overlay :: Overlay
  , tip :: Token
  , struct :: Struct c
  }

setOverlayOn :: Position -> EditView c -> EditView c
setOverlayOn p ev = ev {overlay = Overlay c (Just (H, p))}
  where c = overlayCon $ overlay ev

instance Pane (EditView c) where
  leftClick mpos ev = case s of
    Nothing -> ev --TODO: This should add an orb but will only work after using metachem everywhere
    Just (tkp, opos) -> case toToken tkp of
      Nothing -> ev
      Just tk -> ev {tip = tk}
    where (Overlay c s) = overlay ev
  
  rightClick = setOverlayOn
  
  mouseMove mpos ev = case overlayState o of
    Nothing -> ev
    Just (tk, opos) -> ev {overlay = updateOverlay mpos o}
    where o = overlay ev
