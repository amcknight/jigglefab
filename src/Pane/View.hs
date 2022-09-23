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
import Form
import Time
import Control.Monad.State
import Chem
import Pane.EditView
import Pane.RunView
import Pane.Pane
import Pane.Frame
import Enumer
import Pane.MousePos

data View c = View
  { mode :: Mode
  , editView :: EditView c
  , runView :: RunView c
  , frame :: Frame
  }

data Mode = Edit | Run

instance HasPos (View c) where
  pos = center . frame

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp v = case mode v of
  Edit -> v {mode = Run, runView = rv {model = evalState (buildModel sp (struct ev)) (seed rv)}}
  Run ->  v {mode = Edit, editView = ev {struct = extractStruct $ form $ model rv}}
  where
    ev = editView v
    rv = runView v

lClick :: (Chem c, Enumer c) => MousePos -> View c -> View c
lClick mpos v = case mode v of
  Edit -> v {editView = leftClick (frame v) mpos $ editView v}
  Run  -> v {runView = leftClick (frame v) mpos $ runView v}

rClick :: (Chem c, Enumer c) => MousePos -> View c -> View c
rClick mpos v = case mode v of
  Edit -> v {editView = rightClick (frame v) mpos $ editView v}
  Run  -> v {runView = rightClick (frame v) mpos $ runView v}

mMove :: (Chem c, Enumer c) => MousePos -> View c -> View c
mMove mpos v = case mode v of
  Edit -> v {editView = mouseMove (frame v) mpos $ editView v}
  Run -> v {runView = mouseMove (frame v) mpos $ runView v}
