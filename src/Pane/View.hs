module Pane.View
  ( View(..)
  , Mode(..)
  , initialView
  , panHop, zoomHop
  , togglePlay, setMode
  , leftDown, leftUp, rightDown, rightUp
  , mouseMove, leftDrag, rightDrag
  , viewOrbs
  , menuBlockSize
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
import Enumer
import System.Random (StdGen)
import Geometry.Bound
import Pair

data View c = View
  { mode :: Mode
  , seed :: StdGen
  , screen :: Bound
  , frame :: Frame
  , tip :: c
  , menuHover :: Maybe c
  , orbHover :: Maybe (Orb c)
  , hold :: Maybe (Hold (Orb c))
  , leftClick :: Click
  , rightClick :: Click
  , model :: Model c
  , struct :: Struct c
  }

data Hold a = Hold Position a
data Click = Up | Down
data Mode = Add | Delete | Edit | Move | Run deriving (Eq, Show)

instance HasPos (View c) where
  pos = center . frame

initialView :: forall c . Enumer c => StdGen -> Bound -> Frame -> Model c -> Struct c -> View c
initialView seed scr f = View Run seed scr f (head (vals @c)) Nothing Nothing Nothing Up Up

leftDown :: (Chem c, Enumer c) => MousePos -> View c -> View c
leftDown mpos v' = if inMenu v mpos
  then v {tip = menuChem v mpos}
  else case mode v of
    Add -> v {struct = addOrb (Orb p t) s}
    Delete -> case onOrb of
      Just o -> v {struct = removeOrb o s}
      Nothing -> v
    Edit -> case onOrb of
      Just o -> v {struct = replaceOrb o o{orbChem = t} s}
      Nothing -> v
    Move -> case onOrb of
      Just o -> v {hold = Just $ Hold (pos o |- toAbsPos (frame v) mpos) o}
      Nothing -> v {hold = Nothing}
    Run -> v
  where
    v = v' {leftClick = Down}
    f = frame v
    s = struct v
    t = tip v
    p = toAbsPos f mpos
    onOrb = orbAt s p

leftUp :: MousePos -> View c -> View c
leftUp _ v' = v {hold = Nothing}
  where
    v = v' {leftClick = Up}

rightDown :: MousePos -> View c -> View c
rightDown _ v' = v
  where
    v = v' {rightClick = Down}

rightUp :: MousePos -> View c -> View c
rightUp _ v' = v
  where
    v = v' {rightClick = Up}

mouseMove :: (Eq c, Enumer c) => MousePos -> View c -> View c
mouseMove mpos v = case (leftClick v, rightClick v) of
  (Up, Up) | inMenu v mpos -> v {menuHover = Just $ menuChem v mpos}
           | otherwise -> v {orbHover = orbAt (struct v) (toAbsPos (frame v) mpos)}
  (Down, Up) -> leftDrag mpos v
  (Up, Down) -> rightDrag mpos v
  (Down, Down) -> v

leftDrag :: Eq c => MousePos -> View c -> View c
leftDrag mpos v = case mode v of
  Move -> case hold v of
    Just (Hold dp o) -> let newO = o{orbPos = toAbsPos (frame v) mpos |+ dp}
      in v {hold = Just $ Hold dp newO, struct = replaceOrb o newO (struct v)}
    Nothing -> v
  _ -> v

rightDrag :: MousePos -> View c -> View c
rightDrag _ v = v

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

menuBlockSize :: Double
menuBlockSize = 40

menuTopLeft :: View c -> P Double
menuTopLeft v = (maxX s + menuBlockSize, minY s - menuBlockSize)
  where s = screen v

inMenu :: View c -> MousePos -> Bool
inMenu v (MousePos (mx, my)) = case mode v of
  Add -> inMenuBounds
  Edit -> inMenuBounds
  _ -> False
  where
    (menuX, menuY) = menuTopLeft v
    menuWidth = menuBlockSize * 12
    menuHeight = menuBlockSize * chemSize
    inMenuBounds = mx > menuX && mx < menuX + menuWidth && my < menuY && my > menuY - menuHeight
    chemSize = 20 -- TODO: This should be variable and requires EditView to use metachem. When this is possible, should also probably switch to storing tokens in EditView instead of indices

menuChem :: forall c . Enumer c => View c -> MousePos -> c
menuChem v (MousePos (_, my)) = (vals @c) !! floor ((menuY - my) / menuBlockSize)
  where (_, menuY) = menuTopLeft v

viewOrbs :: View c -> [Orb c]
viewOrbs v = orbs $ case mode v of
  Run -> extractStruct $ form $ model v
  _ -> struct v
