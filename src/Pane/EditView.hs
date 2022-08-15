module Pane.EditView
( EditView(..)
) where

import Struct
import Geometry.Vector
import DataType
import Pane.Pane
import Debug.Trace

data EditView c = EditView
  { tip :: Int
  , hover :: Maybe Int
  , struct :: Struct c
  }

instance Pane (EditView c) where
  leftClick mpos ev = if inMenu mpos && i < chemSize
    then ev {tip = i}
    else ev
    where
      i = menuIndex mpos
      chemSize = 20 -- TODO: This should be variable based on chem size. When this is possible, should also probably switch to storing tokens in EditView instead of indices
  rightClick mpos ev = ev
  mouseMove mpos ev = if inMenu mpos
    then ev {hover = Just i}
    else ev {hover = Nothing}
    where i = menuIndex mpos

-- TODO: Remove magic numbers
inMenu :: Position -> Bool
inMenu (mx, my) = mx > -1880 && mx < -1420 && my < 1020
menuIndex :: Position -> Int
menuIndex (_, my) = floor $ (1020 - my) / 40
