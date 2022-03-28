module Overlay
( Overlay(..)
, overlayThinkness
, updateOverlay
) where

import Geometry.Vector
import Chem
import Geometry.Angle
import Pair
import Debug.Trace

data Overlay = NoOverlay | Overlay Position Token

overlayThinkness :: Double
overlayThinkness = 150

updateOverlay :: Type -> Position -> Overlay -> Overlay
updateOverlay ty mpos NoOverlay = NoOverlay
updateOverlay ty mpos o@(Overlay opos tk) = case dir of
  Nothing -> o
  Just justDir -> case compare layer (length tk) of
    GT -> NoOverlay
    _ -> if minDir <= justDir && maxDir >= justDir
        then Overlay opos tkPrefix
        else NoOverlay
  where
    (minDir, maxDir) = overlayRange (Con "" [ty]) tkPrefix (0,1)
    tkPrefix = take layer tk
    layer = ceiling $ mag / overlayThinkness
    dir = direction v
    mag = magnitude v
    v = mpos |- opos

overlayRange :: Con -> Token -> P Turn -> P Turn
overlayRange c [] r = r
overlayRange c (n:ns) (from, to) = case conI c n of
  Nothing -> error $ "Searching for non-existent chemical token index: " ++ n
  Just i -> case con c n of
    Nothing -> error $ "Searching for non-existent chemical token: " ++ n
    Just nextC -> let
      step = (to - from) / fromIntegral (length (subcons c))
      newFrom = from + fromIntegral i * step
      newTo = newFrom + step
      in overlayRange nextC ns (newFrom, newTo)
