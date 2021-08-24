module ModelLibrary
( linChainModel
, fourChains
, linModel
) where

import Model
import Point
import Chem
import Geometry.Space
import FormLibrary
import Utils

fourChains :: Chem c => Radius -> c -> R (Model c)
fourChains rad ch = do
  f1 <- linChainFormIncl rad 150 1 (-800, 800) (-100, 100) ch
  f2 <- linChainFormIncl rad 150 1 ( 800, 800) ( 100, 100) ch
  f3 <- linChainFormIncl rad 150 1 ( 800,-800) ( 100,-100) ch
  f4 <- linChainFormIncl rad 150 1 (-800,-800) (-100,-100) ch
  pure $ buildModel rad $ box (-1000,-1000) (1000, 1000) <> f1 <> f2 <> f3 <> f4

linChainModel :: Chem c => Radius -> Position -> Position -> c -> R (Model c)
linChainModel rad from to ch = do
  c <- linChainFormIncl rad 150 1 from to ch
  pure $ buildModel rad $ box (-300,-1000) (1000, 500) <> c

linModel :: Chem c => Radius -> Position -> Position -> Int -> c -> R (Model c)
linModel rad from to num ch = do
  f <- linFormIncl 150 from to num ch
  pure $ buildModel rad $ box (-300,-1000) (1000, 500) <> f
