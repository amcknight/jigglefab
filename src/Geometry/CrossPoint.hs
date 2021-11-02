module Geometry.CrossPoint
( CrossPoints(..)
, HasCrossPoints, crossPoints
) where
import Geometry.Vector

data CrossPoints = NoCross | OneCross Position | TwoCross Position Position | InfinteCross deriving (Show, Eq)

class HasCrossPoints a where
  -- Should be sorted by x
  crossPoints :: a -> a -> CrossPoints
