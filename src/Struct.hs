module Struct
( Struct(..)
, wallStruct, orbStruct
) where

import Wall
import Orb

data Struct c = Struct
  { structWalls :: [Wall]
  , orbs :: [Orb c]
  } deriving Show

instance Semigroup (Struct c) where
  (<>) (Struct ws1 os1) (Struct ws2 os2) = Struct (ws1 <> ws2) (os1 <> os2)
instance Monoid (Struct c) where
  mempty = Struct [] []

wallStruct :: Wall -> Struct c
wallStruct w = Struct [w] []

orbStruct :: Orb c -> Struct c
orbStruct o = Struct [] [o]
