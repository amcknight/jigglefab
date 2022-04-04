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
  Just justDir -> case compare layer $ length tk of
    GT -> case expandTk c (overlayRange c tk) justDir tk of
      Nothing -> NoOverlay
      Just newTk -> Overlay opos newTk
    EQ -> case expandTk c (overlayRange c (init tk)) justDir (init tk) of
      Nothing -> NoOverlay
      Just newTk -> Overlay opos newTk
    LT -> Overlay opos $ init tk
  where
    layer = ceiling $ mag / overlayThinkness
    dir = direction v
    mag = magnitude v
    v = mpos |- opos

expandTk :: Con -> P Turn -> Turn -> Token -> Maybe Token
expandTk c (from, to) dir tk = case getCon c tk of
  Nothing -> error $ "Type Error in Token: " ++ show tk
  Just con -> let
    nameI = floor $ fromIntegral (length names) * (dir - from) / (to - from)
    names = conNames con
    in if from <= dir && dir <= to
      then Just $ tk ++ (\ns -> [ns!!nameI]) names
      else Nothing

overlayRange :: Con -> Token -> P Turn
overlayRange c tk = overlayRange' c tk (0,1)
overlayRange' :: Con -> Token -> P Turn -> P Turn
overlayRange' c [] r = r
overlayRange' c (n:ns) (from, to) = case con c n of
  Nothing -> error $ "Searching for non-existent chemical token: " ++ n
  Just (i, nextC) -> let
    step = (to - from) / fromIntegral (length (subcons c))
    newFrom = from + fromIntegral i * step
    newTo = newFrom + step
    in overlayRange' nextC ns (newFrom, newTo)
