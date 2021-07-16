module PHit
( PHit (PHit)
, earliest
, tightenPHit
) where

import Space
import Time
import Point
import Points
import Data.Heap

data PHit = PHit Interval Side Point (Int, Int)

-- movePHit :: Duration -> PHit -> PHit
-- movePHit dt (PHit i s p ids) = PHit (moveInterval dt i) s (movePoint dt p) ids

earliest :: PHit -> Time 
earliest (PHit (lowT, _) _ _ _) = lowT

tightenPHit :: Float -> PHit -> PHit
tightenPHit radSq (PHit i s p ids) = PHit (tightenInterval radSq s p i) s p ids
