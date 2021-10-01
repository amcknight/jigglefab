module Core.CoreStruct
( gateStruct
, meshStruct
) where

import qualified Data.Vector as V
import Core.Core
import Wall
import Point
import StructLibrary
import Pair
import Geometry.Angle
import Struct
import Orb

gateStruct :: Int -> Core -> Struct Core
gateStruct slack c = rocks <> signals <> chains <> gate
  where
    rocks = wallStruct (Circle in1V rad) <> wallStruct (Circle in2V rad) <> wallStruct (Circle outV rad)
    s1 = orbStruct $ Orb in1V $ Wire $ On Red
    s2 = orbStruct $ Orb in2V $ Wire $ On Red
    signals = s1 <> s2
    ch1 = linChainExcl slack in1V gateV $ Wire Off
    ch2 = linChainExcl slack in2V gateV $ Wire Off
    ch3 = linChainExcl slack outV gateV $ Wire Off
    chains = ch1 <> ch2 <> ch3
    gate = orbStruct $ Orb gateV c
    in1V = (-d,-d)
    in2V = (d,-d)
    gateV = (0, 0)
    outV = (0, d)
    d = 500
    rad = 50

meshStruct :: Int -> [(Position, Core)] -> [P Int] -> [(Angle, P Int)] -> Struct Core
meshStruct slack preBalls bbi abbi = pegs <> linCs <> arcCs
  where
    pegs = scatterStruct preBalls
    linCs = linChainsStruct slack (V.fromList preBalls) bbi
    arcCs = arcChainsStruct slack (V.fromList preBalls) abbi

scatterStruct :: [(Position, Core)] -> Struct Core
scatterStruct [] = mempty
scatterStruct ((p,c):pcs) = orbStruct (Orb p c) <> scatterStruct pcs

linChainsStruct :: Int -> V.Vector (Position, Core) -> [P Int] -> Struct Core
linChainsStruct _ _ [] = mempty
linChainsStruct slack preBalls ((i,j):is) = c <> cs
  where
    (p1,_) = preBalls V.! i
    (p2,_) = preBalls V.! j
    c = linChainExcl slack p1 p2 $ Wire Off
    cs = linChainsStruct slack preBalls is

arcChainsStruct :: Int -> V.Vector (Position, Core) -> [(Angle, P Int)] -> Struct Core
arcChainsStruct _ _ [] = mempty
arcChainsStruct slack preBalls ((a,(i,j)):is) = c <> cs
  where
    (p1,_) = preBalls V.! i
    (p2,_) = preBalls V.! j
    c = arcChainExcl a slack p1 p2 $ Wire Off
    cs = arcChainsStruct slack preBalls is

