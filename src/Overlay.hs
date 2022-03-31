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
import DataType

data Overlay = NoOverlay | Overlay Position Token

overlayThinkness :: Double
overlayThinkness = 150

updateOverlay :: Con -> Position -> Overlay -> Overlay
updateOverlay c mpos NoOverlay = NoOverlay
updateOverlay c mpos o@(Overlay opos tk) = case dir of
  Nothing -> o
  Just justDir -> case compare layer (length tk + 1) of
    GT -> NoOverlay
    EQ -> if minDir <= justDir && maxDir >= justDir 
          then Overlay opos $ case expandTk c tk (minDir, maxDir) justDir of
             Nothing -> error "Impossible case since this is covered by above conditions"
             Just newTk -> newTk
          else NoOverlay
    LT -> if minDir <= justDir && maxDir >= justDir
          then Overlay opos tkPrefix
          else NoOverlay
  where
    (minDir, maxDir) = overlayRange c tkPrefix (0,1)
    tkPrefix = take layer tk
    layer = ceiling $ mag / overlayThinkness
    dir = direction v
    mag = magnitude v
    v = mpos |- opos

expandTk :: Con -> Token -> P Turn -> Turn -> Maybe Token
expandTk c tk (from, to) dir = if from <= dir && dir <= to
  then Just $ tk ++ case mbNames of
    Nothing -> []
    Just ns -> [ns!!nameI]
  else Nothing
  where
    nameI = floor $ fromIntegral numNames * (dir - from) / (to - from)
    numNames = maybe 0 length mbNames
    mbNames = case getCon c tk of
       Nothing -> Nothing
       Just nextC -> Just $ conNames nextC

overlayRange :: Con -> Token -> P Turn -> P Turn
overlayRange c [] r = r
overlayRange c (n:ns) (from, to) = case con c n of
  Nothing -> error $ "Searching for non-existent chemical token: " ++ n
  Just (i, nextC) -> let
    step = (to - from) / fromIntegral (length (subcons c))
    newFrom = from + fromIntegral i * step
    newTo = newFrom + step
    in overlayRange nextC ns (newFrom, newTo)
