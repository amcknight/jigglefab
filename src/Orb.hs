module Orb
( Orb (..)
) where

import Geometry.Vector

data Orb c = Orb
  { orbPos :: Position
  , orbChem :: c
  } deriving Show

instance AnchorPos (Orb c) where
  pos = orbPos