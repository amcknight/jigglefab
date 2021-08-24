module Core.CoreForm
( gateForm
, meshForm
) where

import Utils
import Form
import Core.Core
import Ball
import Wall
import Point
import FormLibrary

import qualified Data.Vector as V
import Pair
import Geometry.Space

gateForm :: Float -> Int -> Core -> R (Form Core)
gateForm speed slack c = do
  let rocks = wallForm (Circle in1V rad) <> wallForm (Circle in2V rad) <> wallForm (Circle outV rad)
  s1 <- ballFormAt speed in1V Active
  s2 <- ballFormAt speed in2V Active
  let signals = s1 <> s2
  ch1 <- chainFormExcl rad speed slack in1V gateV Dormant
  ch2 <- chainFormExcl rad speed slack in2V gateV Dormant
  ch3 <- chainFormExcl rad speed slack outV gateV Dormant
  let chains = ch1 <> ch2 <> ch3
  gate <- ballFormAt speed gateV c
  pure $ rocks <> signals <> chains <> gate
  where
    in1V = (-d,-d)
    in2V = (d,-d)
    gateV = (0, 0)
    outV = (0, d)
    d = 500
    rad = 50

meshForm :: Radius -> Float -> Int -> [(Position, Core)] -> [P Int] -> R (Form Core)
meshForm rad speed slack preBalls bbi = do
  pegs <- scatterForm rad speed preBalls
  chains <- chainsForm rad speed slack (V.fromList preBalls) bbi
  pure $ pegs <> chains

scatterForm :: Radius -> Float -> [(Position, Core)] -> R (Form Core)
scatterForm _ _ [] = do pure mempty
scatterForm rad speed ((p,c):bs) = do
  bForm <- ballFormAt speed p c
  bsForms <- scatterForm rad speed bs
  pure $ bForm <> bsForms

chainsForm :: Radius -> Float -> Int -> V.Vector (Position, Core) -> [P Int] -> R (Form Core)
chainsForm _ _ _ _ [] = do pure mempty
chainsForm rad speed slack preBalls ((i,j):is) = do
  let (p1,_) = preBalls V.! i
  let (p2,_) = preBalls V.! j
  cForm <- chainFormExcl rad speed slack p1 p2 Dormant
  csForms <- chainsForm rad speed slack preBalls is
  pure $ cForm <> csForms
