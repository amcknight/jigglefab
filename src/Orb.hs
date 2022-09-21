module Orb
( Orb (..)
, buildOrb
) where

import Geometry.Vector
import Types

data Orb c = Orb
  { orbPos :: Position
  , orbChem :: c
  } deriving (Show, Eq)

instance HasPos (Orb c) where
  pos :: Orb c -> Position
  pos = orbPos

buildOrb :: Pos 'World -> c -> Orb c
buildOrb = Orb . unsafePosition
