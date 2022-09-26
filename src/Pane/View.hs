module Pane.View
  ( View(..)
  , Mode(..)
  , initialView
  , panHop, zoomHop
  , togglePlay, setMode
  , leftClick, rightClick
  , mouseMove
  ) where

import Model
import Geometry.Vector
import Form
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

data Mode = Add | Delete | Edit | Move | Run deriving (Eq, Show)

instance HasPos (View c) where
  pos = center . frame

initialView :: forall c . Enumer c => StdGen -> Frame -> Model c -> Struct c -> View c
initialView seed f = View Run seed f (head (vals @c)) Nothing Nothing

leftClick :: (Chem c, Enumer c) => MousePos -> View c -> View c
leftClick mpos v = if inMenu (mode v) mpos
  then v {tip = menuChem mpos}
  else case mode v of
    Add -> v {struct = addOrb (Orb pos t) s}
    Delete ->  case onOrb of
      Just o -> v {struct = removeOrb o s}
      Nothing -> v
    Edit -> case onOrb of
      Just o -> v {struct = replaceOrb o o{orbChem = t} s}
      Nothing -> v
    Move -> v
    Run -> v
  where
    f = frame v
    s = struct v
    t = tip v
    pos = toAbsPos f mpos
    onOrb = orbAt s pos

rightClick :: MousePos -> View c -> View c
rightClick _ v = v

mouseMove :: Enumer c => MousePos -> View c -> View c
mouseMove mpos v
  | inMenu (mode v) mpos = v {menuHover = Just $ menuChem mpos}
  | otherwise = v {orbHover = orbAt (struct v) (toAbsPos (frame v) mpos)}

togglePlay :: Chem c => View c -> View c
togglePlay v = case mode v of
  Run -> setMode Edit v
  _ -> setMode Run v

setMode :: Chem c => Mode -> View c -> View c
setMode m v = case (mode v, m) of
  (Run, Run) -> v
  (Run, _) -> v {mode = m, struct = extractStruct $ form $ model v}
  (_, Run) -> v {mode = Run, model = evalState (buildModel (speed (model v)) (struct v)) (seed v)}
  _ -> v {mode = m}

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
