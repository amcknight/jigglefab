module StructLibrary
( cappedLinIncl, cappedLinExcl
, cappedArcIncl
, linIncl, linExcl
, cappedLinChainIncl, cappedLinChainExcl
, linChainIncl, linChainExcl
, arcIncl, arcExcl
, arcChainIncl, arcChainExcl
, sig
, box
) where

import Model.Point
import Geometry.Angle
import Geometry.Vector
import Model.Orb
import Model.Wall
import Model.Struct

box :: Vector -> Vector -> Struct c
box (x1,y1) (x2,y2) =
  wallStruct (VLine x1) <> 
  wallStruct (VLine x2) <> 
  wallStruct (HLine y1) <> 
  wallStruct (HLine y2)

sig :: Position -> Position -> [c] -> Struct c
sig _ _ [] = mempty
sig _ to [ch] = orbStruct $ Orb to ch
sig from to (ch:cs) = form <> forms
  where
    form = orbStruct $ Orb from ch
    gap = (1 / fromIntegral (length cs)) |* (to |- from)
    forms = sig (from |+ gap) to cs

cappedLinIncl :: Position -> Position -> Int -> [c] -> c -> [c] -> Struct c
cappedLinIncl from to num fromCh ch toCh = Struct [] $ zipWith Orb poss chems
  where
    poss = fromTo from to num
    chems = fromCh ++ replicate (num - length fromCh - length toCh) ch ++ reverse toCh

cappedLinExcl :: Position -> Position -> Int -> [c] -> c -> [c] -> Struct c
cappedLinExcl from to num = cappedLinIncl (from |+ step) (to |- step) (num-2)
  where
    numHops = fromIntegral $ num - 1
    step = (1/numHops) |* (to |- from)

cappedArcIncl :: Radian -> Position -> Position -> Int -> [c] -> c -> [c] -> Struct c
cappedArcIncl a from to num fromCh ch toCh = Struct [] $ zipWith Orb poss chems
  where
    poss = arcFromTo a from to num
    chems = fromCh ++ replicate (num - length fromCh - length toCh) ch ++ reverse toCh

linIncl :: Position -> Position -> Int -> c -> Struct c
linIncl from to num ch = cappedLinIncl from to num [] ch []

linExcl :: Position -> Position -> Int -> c -> Struct c
linExcl from to num = linIncl (from |+ step) (to |- step) (num-2)
  where step = (1/fromIntegral (num-1)) |* (to |- from)

arcIncl :: Radian -> Position -> Position -> Int -> c -> Struct c
arcIncl angle from to num ch = cappedArcIncl angle from to num [] ch []

arcExcl :: Radian -> Position -> Position -> Int -> c -> Struct c
arcExcl angle from to num ch = Struct [] orbStructs
  where
    poss = arcFromTo angle from to num
    _:orbStructs = take (num-1) $ fmap (`Orb` ch) poss

cappedLinChainIncl :: Int -> Position -> Position -> [c] -> c -> [c] -> Struct c
cappedLinChainIncl slack from to = cappedLinIncl from to num
  where num = (ceiling (dist from to) + 1) + slack

linChainIncl :: Int -> Position -> Position -> c -> Struct c
linChainIncl slack from to ch = cappedLinChainIncl slack from to [] ch []

cappedLinChainExcl :: Int -> Position -> Position -> [c] -> c -> [c] -> Struct c
cappedLinChainExcl slack from to = cappedLinExcl from to num
  where num = (ceiling (dist from to) + 1) + slack

linChainExcl :: Int -> Position -> Position -> c -> Struct c
linChainExcl slack from to = linExcl from to num
  where num = (ceiling (dist from to) + 1) + slack

arcChainIncl :: Radian -> Int -> Position -> Position -> c -> Struct c
arcChainIncl a slack from to = arcIncl a from to num
  where num = (ceiling (arcDist a from to) + 1) + slack

arcChainExcl :: Radian -> Int -> Position -> Position -> c -> Struct c
arcChainExcl a slack from to = arcExcl a from to num
  where num = (ceiling (arcDist a from to) + 1) + slack
