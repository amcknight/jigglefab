module Orb
( Orb (..)
) where

import Geometry.Vector

data Orb c = Orb
  { orbPos :: Position
  , orbChem :: c
  } deriving (Show, Eq)

instance HasPos (Orb c) where
  pos :: Orb c -> Position
  pos = orbPos
