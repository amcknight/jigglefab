module Types
( Basis(..)
, Pos(..)
) where

import Geometry.Vector

data Basis = Screen | World
newtype Pos (c :: Basis) = UnsafePos
  { unsafePos :: Position }
