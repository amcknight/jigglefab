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
    EQ -> if minDir <= justDir && justDir <= maxDir 
          then Overlay opos $ case expandTk c (minDir, maxDir) justDir tk of
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

expandTk :: Con -> P Turn -> Turn -> Token -> Maybe Token
expandTk c (from, to) dir tk = if from <= dir && dir <= to
  then Just $ tk ++ maybe [] (\ns -> [ns!!nameI]) mbNames
  else Nothing
  where
    nameI = floor $ fromIntegral numNames * (dir - from) / (to - from)
    numNames = maybe 0 length mbNames
    mbNames =conNames <$> getCon c tk
    x = getCon c tk

overlayRange :: Con -> Token -> P Turn -> P Turn
overlayRange c [] r = r
overlayRange c (n:ns) (from, to) = case con c n of
  Nothing -> error $ "Searching for non-existent chemical token: " ++ n
  Just (i, nextC) -> let
    step = (to - from) / fromIntegral (length (subcons c))
    newFrom = from + fromIntegral i * step
    newTo = newFrom + step
    in overlayRange nextC ns (newFrom, newTo)
