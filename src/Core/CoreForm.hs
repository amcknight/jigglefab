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
import Geometry.Angle

gateForm :: Float -> Int -> Core -> R (Form Core)
gateForm speed slack c = do
  let rocks = wallForm (Circle in1V rad) <> wallForm (Circle in2V rad) <> wallForm (Circle outV rad)
  s1 <- ballFormAt speed in1V $ Wire $ On Red
  s2 <- ballFormAt speed in2V $ Wire $ On Red
  let signals = s1 <> s2
  ch1 <- linChainFormExcl rad speed slack in1V gateV $ Wire Off
  ch2 <- linChainFormExcl rad speed slack in2V gateV $ Wire Off
  ch3 <- linChainFormExcl rad speed slack outV gateV $ Wire Off
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

meshForm :: Radius -> Float -> Int -> [(Position, Core)] -> [P Int] -> [(Angle, P Int)] -> R (Form Core)
meshForm rad speed slack preBalls bbi abbi = do
  pegs <- scatterForm rad speed preBalls
  linChains <- linChainsForm rad speed slack (V.fromList preBalls) bbi
  arcChains <- arcChainsForm rad speed slack (V.fromList preBalls) abbi
  pure $ pegs <> linChains <> arcChains

scatterForm :: Radius -> Float -> [(Position, Core)] -> R (Form Core)
scatterForm _ _ [] = do pure mempty
scatterForm rad speed ((p,c):bs) = do
  bForm <- ballFormAt speed p c
  bsForms <- scatterForm rad speed bs
  pure $ bForm <> bsForms

linChainsForm :: Radius -> Float -> Int -> V.Vector (Position, Core) -> [P Int] -> R (Form Core)
linChainsForm _ _ _ _ [] = do pure mempty
linChainsForm rad speed slack preBalls ((i,j):is) = do
  let (p1,_) = preBalls V.! i
  let (p2,_) = preBalls V.! j
  cForm <- linChainFormExcl rad speed slack p1 p2 $ Wire Off
  csForms <- linChainsForm rad speed slack preBalls is
  pure $ cForm <> csForms

arcChainsForm :: Radius -> Float -> Int -> V.Vector (Position, Core) -> [(Angle, P Int)] -> R (Form Core)
arcChainsForm _ _ _ _ [] = do pure mempty
arcChainsForm rad speed slack preBalls ((a,(i,j)):is) = do
  let (p1,_) = preBalls V.! i
  let (p2,_) = preBalls V.! j
  cForm <- arcChainFormExcl rad speed a slack p1 p2 $ Wire Off
  csForms <- arcChainsForm rad speed slack preBalls is
  pure $ cForm <> csForms
