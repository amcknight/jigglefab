module Pane.EditView
( EditView(..)
) where

import Struct
import Geometry.Vector
import Pane.Frame
import Pane.Pane
import Debug.Trace
import Orb
import Enumer
import Chem

data EditView c = EditView
  { tip :: Int
  , menuHover :: Maybe Int
  , orbHover :: Maybe (Orb c) -- TODO: This should be a polygon or an identifier for an Orb
  , struct :: Struct c
  }

instance Pane (EditView c) where
  leftClick frame mpos ev = if inMenu mpos
    then ev {tip = i}
    else ev --{struct = addOrb (Orb mpos (tokenFromI ch (tip ev))) (struct ev)}
    where i = menuIndex mpos
  rightClick frame mpos ev = ev
  mouseMove frame mpos ev
    | inMenu mpos = ev {menuHover = Just $ menuIndex mpos}
    | otherwise = ev {orbHover = orbAt (struct ev) (toAbsPos frame mpos)}

menuItemHeight :: Double
menuItemHeight = 40

-- TODO: Remove magic numbers
inMenu :: Position -> Bool
inMenu (mx, my) = mx > -1880 && mx < -1420 && my < 1020 && my > 1020 - menuItemHeight*chemSize
  where
    chemSize = 20 -- TODO: This should be variable and requires EditView to use metachem. When this is possible, should also probably switch to storing tokens in EditView instead of indices

menuIndex :: Position -> Int
menuIndex (_, my) = floor $ (1020 - my) / menuItemHeight
