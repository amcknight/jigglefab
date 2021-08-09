module ModelLibrary
( chainModel
, fourChains
, randomLinearModel
, wireModel
) where

import Model
import Point
import Chem
import Vector
import Space
import Wall
import FormLibrary
import Form
import Utils

fourChains :: Chem c => Radius -> c -> R (Model c)
fourChains rad ch = do
  f1 <- chainForm rad 150 1 (V (-800)   800)  (V (-100)   100)  ch
  f2 <- chainForm rad 150 1 (V   800    800)  (V   100    100)  ch
  f3 <- chainForm rad 150 1 (V   800  (-800)) (V   100  (-100)) ch
  f4 <- chainForm rad 150 1 (V (-800) (-800)) (V (-100) (-100)) ch
  pure $ buildModel rad $ box (V (-1000) (-1000)) (V 1000 1000) <> f1 <> f2 <> f3 <> f4

chainModel :: Chem c => Radius -> Position -> Position -> c -> R (Model c)
chainModel rad from to ch = do
  c <- chainForm rad 150 1 from to ch
  pure $ buildModel rad $ box (V (-300) (-1000)) (V 1000 500) <> c

randomLinearModel :: Chem c => Radius -> Position -> Position -> Int -> c -> R (Model c)
randomLinearModel rad from to num ch = do
  f <- randomLinearForm 150 from to num ch
  pure $ buildModel rad $ box (V (-300) (-1000)) (V 1000 500) <> f

wireModel :: Chem c => Radius -> Float -> Vector -> Vector -> c -> R (Model c)
wireModel rad speed (V x1 y1) (V x2 y2) ch = do
  let walls = wallForm (wallV (damp x1)) <> wallForm (wallH (damp y1)) <> wallForm (wallV (damp x2)) <> wallForm (wallH (damp y2))
  chain <- chainForm rad speed 4 (V x1 y1) (V x2 y2) ch
  pure $ buildModel rad $ walls <> chain
  where 
    damp x = case compare x 0 of
      LT -> x + 1
      EQ -> x
      GT -> x - 1
