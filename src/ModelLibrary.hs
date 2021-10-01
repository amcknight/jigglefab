module ModelLibrary
( linChainModel
, fourChains
, linModel
) where

import Model
import Point
import Chem
import Geometry.Space
import StructLibrary
import Utils

fourChains :: Chem c => c -> R (Model c)
fourChains ch = do
  let s1 = linChainIncl 1 (-800, 800) (-100, 100) ch
  let s2 = linChainIncl 1 ( 800, 800) ( 100, 100) ch
  let s3 = linChainIncl 1 ( 800,-800) ( 100,-100) ch
  let s4 = linChainIncl 1 (-800,-800) (-100,-100) ch
  buildModel 2 $ box (-1000,-1000) (1000, 1000) <> s1 <> s2 <> s3 <> s4

linChainModel :: Chem c => Position -> Position -> c -> R (Model c)
linChainModel from to ch = do
  let c = linChainIncl 1 from to ch
  buildModel 2 $ box (-300,-1000) (1000, 500) <> c

linModel :: Chem c => Position -> Position -> Int -> c -> R (Model c)
linModel from to num ch = do
  let f = linIncl from to num ch
  buildModel 2 $ box (-300,-1000) (1000, 500) <> f
