module Pane.View
  ( View(..)
  , Mode(..)
  , initialView
  , panHop, zoomHop
  , togglePlay
  , leftClick, rightClick
  , mouseMove
  ) where

import Model
import Geometry.Vector
import Form
import Time
import Control.Monad.State
import Chem
import Pane.Frame
import Pane.MousePos
import Struct
import Orb
import System.Random (StdGen)
import Enumer

data View c = View
  { mode :: Mode
  , seed :: StdGen
  , frame :: Frame
  , tip :: c
  , menuHover :: Maybe c
  , orbHover :: Maybe (Orb c)
  , model :: Model c
  , struct :: Struct c
  }

data Mode = Add | Delete | Edit | Move | Run

instance HasPos (View c) where
  pos = center . frame

initialView :: forall c . Enumer c => StdGen -> Frame -> Model c -> Struct c -> View c
initialView seed f = View Run seed f (head (vals @c)) Nothing Nothing

leftClick :: (Chem c, Enumer c) => Frame -> MousePos -> View c -> View c
leftClick frame mpos v = if inMenu mpos
  then v {tip = c}
  else case orbAt (struct v) (toAbsPos frame mpos) of
    Nothing -> v {struct = addOrb (Orb (toAbsPos frame mpos) (tip v)) (struct v)}
    Just o -> v {struct = replaceOrb (struct v) o o {orbChem = tip v} }
  where
    c = menuChem mpos

rightClick :: Frame -> MousePos -> View c -> View c
rightClick _ _ ev = ev

mouseMove :: Enumer c => Frame -> MousePos -> View c -> View c
mouseMove frame mpos ev
  | inMenu mpos = ev {menuHover = Just $ menuChem mpos}
  | otherwise = ev {orbHover = orbAt (struct ev) (toAbsPos frame mpos)}

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp v = case mode v of
  Run ->  v {mode = Edit, struct = extractStruct $ form $ model v}
  _ -> v {mode = Run, model = evalState (buildModel sp (struct v)) (seed v)}

-- TODO: Remove magic numbers
menuItemHeight :: Double
menuItemHeight = 40

inMenu :: MousePos -> Bool
inMenu (MousePos (mx, my)) = mx > -1880 && mx < -1420 && my < 1020 && my > 1020 - menuItemHeight*chemSize
  where
    chemSize = 20 -- TODO: This should be variable and requires EditView to use metachem. When this is possible, should also probably switch to storing tokens in EditView instead of indices

menuChem :: forall c . Enumer c => MousePos -> c
menuChem (MousePos (_, my)) = (vals @c) !! floor ((1020 - my) / menuItemHeight)
