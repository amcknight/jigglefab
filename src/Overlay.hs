module Overlay
( Overlay(..)
, overlayThickness
, updateOverlay
) where

import Geometry.Vector
import Geometry.Angle
import Pair
import DataType

data Overlay = Overlay
  { overlayCon :: Con
  , overlayState :: Maybe (TkPart, Position)
  }

overlayThickness :: Double
overlayThickness = 150

updateOverlay :: Position -> Overlay -> Overlay
updateOverlay mpos o@(Overlay c s) = case s of
  Nothing -> Overlay c Nothing
  Just (tkp, opos) -> let
    layer = ceiling $ mag / overlayThickness
    dir = direction v
    mag = magnitude v
    v = mpos |- opos
    in case dir of
      Nothing -> o
      Just justDir -> updateOverlay' c tkp opos layer

updateOverlay' :: Con -> TkPart -> Position -> Int -> Overlay
updateOverlay' c tkp opos layer = case compare layer $ numNames tkp of
  GT -> case extendTkPart c tkp of
    Nothing -> Overlay c Nothing
    Just newTkp -> Overlay c $ Just (newTkp, opos)
  EQ -> case extendTkPart c reducedTkp of
    Nothing -> Overlay c Nothing
    Just newTkp -> Overlay c $ Just (newTkp, opos)
  LT -> Overlay c $ Just (reducedTkp, opos)
  where 
    reducedTkp = case reduceTkPart tkp of
      Nothing -> error "Trying to reduce tkp when unreducable"
      Just t -> t

  -- GT -> case extendTkPartByPos c (overlayRange c tkp) justDir tkp of
  --   Nothing -> Overlay c Nothing
  --   Just newTkp -> Overlay c $ Just (newTkp, opos)
  -- EQ -> case extendTkPartByPos c (overlayRange c reducedTkp) justDir reducedTkp of
  --   Nothing -> Overlay c Nothing
  --   Just newTkp -> Overlay c $ Just (newTkp, opos)
  -- LT -> Overlay c $ Just (reducedTkp, opos)
  -- where reducedTkp = reduceTkPart tkp

-- overlayRange :: Con -> Token -> P Turn
-- overlayRange c tk = overlayRange' c tk (0,1)
-- overlayRange' :: Con -> Token -> P Turn -> P Turn
-- overlayRange' c [] r = r
-- overlayRange' c (n:ns) (from, to) = case con c n of
--   Nothing -> error $ "Searching for non-existent chemical token: " ++ n
--   Just (i, nextC) -> let
--     step = (to - from) / fromIntegral (length (subcons c))
--     newFrom = from + fromIntegral i * step
--     newTo = newFrom + step
--     in overlayRange' nextC ns (newFrom, newTo)
