module ModelLibrary
( chainModel
, fourChains
, randomLinearModel
) where

import Model
import Point
import Chem
import Geometry.Space
import FormLibrary
import Utils

fourChains :: Chem c => Radius -> c -> R (Model c)
fourChains rad ch = do
  f1 <- chainFormIncl rad 150 1 (-800, 800) (-100, 100) ch
  f2 <- chainFormIncl rad 150 1 ( 800, 800) ( 100, 100) ch
  f3 <- chainFormIncl rad 150 1 ( 800,-800) ( 100,-100) ch
  f4 <- chainFormIncl rad 150 1 (-800,-800) (-100,-100) ch
  pure $ buildModel rad $ box (-1000,-1000) (1000, 1000) <> f1 <> f2 <> f3 <> f4

chainModel :: Chem c => Radius -> Position -> Position -> c -> R (Model c)
chainModel rad from to ch = do
  c <- chainFormIncl rad 150 1 from to ch
  pure $ buildModel rad $ box (-300,-1000) (1000, 500) <> c

randomLinearModel :: Chem c => Radius -> Position -> Position -> Int -> c -> R (Model c)
randomLinearModel rad from to num ch = do
  f <- linearFormIncl 150 from to num ch
  pure $ buildModel rad $ box (-300,-1000) (1000, 500) <> f
