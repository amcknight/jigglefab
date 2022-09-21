module Pane.EditView
( EditView(..)
) where

import Struct
import Pane.Frame
import Pane.Pane
import Orb
import Enumer
import Chem
import Types

data EditView c = EditView
  { tip :: Int -- TODO: Should this just BE a Chem?
  , menuHover :: Maybe Int
  , orbHover :: Maybe (Orb c)
  , struct :: Struct c
  }

instance (Chem c, Enumer c) => Pane (EditView c) where
  leftClick :: Frame -> Pos 'Screen -> EditView c -> EditView c
  leftClick frame mpos ev = if inMenu mpos
    then ev {tip = i}
    else case orbAt (struct ev) (toAbsPos frame mpos) of
      Nothing -> ev {struct = addOrb (buildOrb (toAbsPos frame mpos) ((vals @c) !! tip ev)) (struct ev)}
      Just o -> ev {struct = replaceOrb (struct ev) o o {orbChem = (vals @c) !! tip ev} }
    where
      i = menuIndex mpos
  
  rightClick :: Frame -> Pos 'Screen -> EditView c -> EditView c
  rightClick _ _ ev = ev

  mouseMove :: Frame -> Pos 'Screen -> EditView c -> EditView c
  mouseMove frame mpos ev
    | inMenu mpos = ev {menuHover = Just $ menuIndex mpos}
    | otherwise = ev {orbHover = orbAt (struct ev) (toAbsPos frame mpos)}

menuItemHeight :: Double
menuItemHeight = 40

-- TODO: Remove magic numbers
inMenu :: Pos 'Screen -> Bool
inMenu (UnsafePos (mx, my)) = mx > -1880 && mx < -1420 && my < 1020 && my > 1020 - menuItemHeight*chemSize
  where
    chemSize = 20 -- TODO: This should be variable and requires EditView to use metachem. When this is possible, should also probably switch to storing tokens in EditView instead of indices

menuIndex :: Pos 'Screen -> Int
menuIndex (UnsafePos (_, my)) = floor $ (1020 - my) / menuItemHeight
