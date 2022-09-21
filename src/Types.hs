module Types where
import Geometry.Vector
import qualified Geometry.Vector as X
import Data.Coerce

data Basis = Screen | World
newtype Pos (c :: Basis) = UnsafePos
  { unsafePosition :: Position }

(|*) :: Double -> Pos c -> Pos c
(|*) = coerce (X.|*)
