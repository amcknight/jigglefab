module Pane.View
  ( View(..)
  , Mode(..)
  , panHop, zoomHop
  , togglePlay
  , lClick, rClick
  , mMove
  ) where

import Model
import Geometry.Vector
import Geometry.Space
import Form
import Time
import Control.Monad.State
import Chem
import DataType
import Pane.EditView
import Pane.RunView
import Pane.Pane
import Debug.Trace
import qualified Control.Arrow as Pane.Pane

data View c = View
  { mode :: Mode
  , editView :: EditView c
  , runView :: RunView c
  , center :: Position
  , zoom :: Double
  }

data Mode = Edit | Run

instance HasPos (View c) where
  pos = center

panHop :: Geometry.Vector.Vector -> View c -> View c
panHop dirV view = view {center = center view |+ (hop |* dirV)}
  where hop = 150

zoomHop :: Side -> View c -> View c
zoomHop s view = case s of
  Out -> view {zoom = zoom view * zhop}
  In -> view {zoom = zoom view * (1/zhop)}
  where zhop = 1.25

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp v = case mode v of
  Edit -> v {mode = Run, runView = rv {model = evalState (buildModel sp (struct ev)) (seed rv)}}
  Run ->  v {mode = Edit, editView = ev {struct = extractStruct $ form $ model rv}}
  where
    ev = editView v
    rv = runView v

lClick :: Position -> Con -> View c -> View c
lClick mpos c v = case mode v of
  Edit -> v {editView = leftClick mpos $ editView v}
  Run  -> v {runView = leftClick mpos $ runView v}

rClick :: Position -> View c -> View c
rClick mpos v = case mode v of
  Edit -> v {editView = rightClick mpos $ editView v}
  Run  -> v {runView = rightClick mpos $ runView v}

mMove :: Position -> Con -> View c -> View c
mMove mpos c v = case mode v of
  Edit -> v {editView = mouseMove mpos $ editView v}
  Run -> v {runView = mouseMove mpos $ runView v}
