module Pane.View
  ( View(..)
  , Mode(..)
  , panHop, zoomHop
  , togglePlay
  , setOverlayOn
  , click, rightClick
  , mouseMove
  ) where

import Model
import Geometry.Vector
import Geometry.Space
import Form
import Time
import Control.Monad.State
import Chem
import Overlay
import DataType
import Pane.EditView
import Pane.RunView

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
  Edit -> v {mode = Run, runView = (runView v) {model = evalState (buildModel sp (struct (editView v))) (seed (runView v))}}
  Run ->  v {mode = Edit, editView = (editView v) {overlay = NoOverlay, struct = extractStruct $ form $ model $ runView v}}

click :: Position -> Con -> View c -> View c
click mpos c v = case mode v of
  Edit -> let ev = editView v
    in case overlay ev of
      NoOverlay -> case getCon c $ tip ev of
        Nothing -> error $ "Invalid TIP: " ++ show (tip ev)
        Just con -> v -- v {style = Left $ addOrb (Orb mpos con) st} TODO: This will only work if using Metachem instead of chem everywhere
      Overlay _ tk -> if isLeafAt c tk
        then v {editView = ev {tip = tk}}
        else v
  _ -> v

rightClick :: Position -> View c -> View c
rightClick mpos v = case mode v of
  Edit -> v {editView = setOverlayOn mpos $ editView v}
  _ -> v

mouseMove :: Position -> Con -> View c -> View c
mouseMove mpos c v = case mode v of
  Edit -> let ev = editView v
    in case overlay ev of
      NoOverlay -> v
      Overlay _ _ -> v {editView = ev {overlay = updateOverlay c mpos $ overlay ev}}
  _ -> v
