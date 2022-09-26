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

leftClick :: (Chem c, Enumer c) => MousePos -> View c -> View c
leftClick mpos v = if inMenu (mode v) mpos
  then v {tip = c}
  else case orbAt (struct v) (toAbsPos f mpos) of
    Nothing -> v {struct = addOrb (Orb (toAbsPos f mpos) (tip v)) (struct v)}
    Just o -> v {struct = replaceOrb (struct v) o o {orbChem = tip v} }
  where
    f = frame v
    c = menuChem mpos

rightClick :: MousePos -> View c -> View c
rightClick _ v = v

mouseMove :: Enumer c => MousePos -> View c -> View c
mouseMove mpos v
  | inMenu (mode v) mpos = v {menuHover = Just $ menuChem mpos}
  | otherwise = v {orbHover = orbAt (struct v) (toAbsPos (frame v) mpos)}

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp v = case mode v of
  Run ->  v {mode = Edit, struct = extractStruct $ form $ model v}
  _ -> v {mode = Run, model = evalState (buildModel sp (struct v)) (seed v)}

-- TODO: Remove magic numbers
menuItemHeight :: Double
menuItemHeight = 40

inMenu :: Mode -> MousePos -> Bool
inMenu m (MousePos (mx, my)) = case m of
  Add -> inMenuBounds
  Edit -> inMenuBounds
  _ -> False
  where
    inMenuBounds = mx > -1880 && mx < -1420 && my < 1020 && my > 1020 - menuItemHeight*chemSize
    chemSize = 20 -- TODO: This should be variable and requires EditView to use metachem. When this is possible, should also probably switch to storing tokens in EditView instead of indices

menuChem :: forall c . Enumer c => MousePos -> c
menuChem (MousePos (_, my)) = (vals @c) !! floor ((1020 - my) / menuItemHeight)
