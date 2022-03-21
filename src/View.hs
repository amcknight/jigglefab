module View
  ( View(..)
  , panHop, zoomHop
  , togglePlay
  )
where

import Model
import Geometry.Vector
import Struct
import Geometry.Space
import Form
import System.Random
import Time
import Control.Monad.State
import Chem
import Debug.Trace

data View c = View
  { structOrModel :: Either (Struct c) (Model c)
  , seed :: StdGen
  , center :: Position
  , zoom :: Double
  }

instance HasPos (View c) where
  pos = center

panHop :: Geometry.Vector.Vector -> View c -> View c
panHop dirV view = view {center = center view |- (hop |* dirV)}
  where hop = 150

zoomHop :: Side -> View c -> View c
zoomHop s view = case s of
  Out -> view {zoom = zoom view * zhop}
  In -> view {zoom = zoom view * (-zhop)}
  where zhop = 1.25

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp view = case structOrModel view of
  Left s -> view { structOrModel = Right (evalState (buildModel sp s) (seed view)) }
  Right m -> view { structOrModel = Left (extractStruct (form m)) }
